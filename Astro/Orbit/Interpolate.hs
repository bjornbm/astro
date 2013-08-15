{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Astro.Orbit.Interpolate where

import Astro
import Astro.Time
import Astrodynamics
import Astro.Coords (MEGSD)
import Astro.Coords.PosVel
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional (Dimensional (Dimensional))
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.PosVel hiding (longitude)

import Astro.Orbit.Types
import Astro.Orbit.MEOE
import Astro.Time.At
import Astro.Trajectory (Datum)
import qualified Prelude as P

import Control.Applicative


-- dimensional
-- -----------
-- Interpolate.
linearPolate :: (Div d d DOne, Mul DOne dy dy, Fractional a)
             => (Quantity d a, Quantity dy a) -> (Quantity d a, Quantity dy a)
             -> Quantity d a -> Quantity dy a
linearPolate (x0,y0) (x1,y1) x = linearPolate0 y0 (x1-x0,y1) (x-x0)

-- Polate assuming y0 corresponds to x0 = 0.
linearPolate0 :: (Div d d DOne, Mul DOne dy dy, Fractional a)
             => Quantity dy a -> (Quantity d a, Quantity dy a)
             -> Quantity d a -> Quantity dy a
--linearPolate0 y0 (x1,y1) x = y0 + x / x1 * (y1 - y0)
linearPolate0 y0 (x1,y1) x = y0 + linearPolate00 (x1,y1-y0) x

-- Polate assuming x0 = 0 and y0 = 0.
linearPolate00 :: (Div d d DOne, Mul DOne dy dy, Fractional a)
             => (Quantity d a, Quantity dy a)
             -> Quantity d a -> Quantity dy a
linearPolate00 (x1,y1) x = x / x1 * y1

-- dimensional-vectors
-- -------------------
-- Interpolate vector.
linearPolateVec :: (Div d d DOne, Fractional a)
                => (Quantity d a, Vec ds a) -> (Quantity d a, Vec ds a)
                -> Quantity d a -> Vec ds a
linearPolateVec (x0,v0) (x1,v1) x = linearPolateVec0 v0 (x1-x0,v1) (x-x0)

-- Polate assuming v0 corresponds to x0 = 0.
linearPolateVec0 :: (Div d d DOne, Fractional a)
                 => Vec ds a -> (Quantity d a, Vec ds a)
                 -> Quantity d a -> Vec ds a
--linearPolateVec0 v0 (x1,v1) x = v0 >+< scaleVec1 (x / x1) (v1 >-< v0)
linearPolateVec0 v0 (x1,v1) x = v0 >+< linearPolateVec00 (x1, v1 >-< v0) x

-- Polate assuming x0 = 0 and v0 = {0}.
linearPolateVec00 :: (Div d d DOne, Fractional a)
                 => (Quantity d a, Vec ds a)
                 -> Quantity d a -> Vec ds a
linearPolateVec00 (x1,v1) x = scaleVec1 (x / x1) v1


-- Depend on astro
-- ---------------

-- | Interpolate or extrapolate linearly.
linearPolateT :: (Mul DOne d d, Fractional a)
              => At t a (Quantity d a) -> At t a (Quantity d a)
              -> E t a -> Quantity d a
linearPolateT (x0`At`t0) (x1`At`t1) t = linearPolate0 x0 (d t1,x1) (d t)
  where d = (`diffEpoch` t0)

-- Interpolate vector as function of time.
linearPolateVecT :: Fractional a
                 => At t a (Vec ds a) -> At t a (Vec ds a) -> E t a -> Vec ds a
linearPolateVecT (v0`At`t0) (v1`At`t1) t = linearPolateVec0 v0 (d t1,v1) (d t)
  where d = (`diffEpoch` t0)

-- ==================================================================

-- | Interpolate two MEOEs with mean anomaly. In interpolating mean
-- anomaly the orbital period is taken into account to ensure proper
-- interpolation.
-- NOTE: this does not work for hyperbolic orbits, and probably not
-- when t1 and t0 are separated by more than one (or a half?) orbital
-- period?
-- {-
linearPolateMEOEm :: RealFloat a
                  => Datum t a -> Datum t a
                  -> E t a -> MEOE Mean a
linearPolateMEOEm (m0`At`t0) (m1`At`t1) t = ( vec2meoe
  $ linearPolateVecT (meoe2vec m0`At`t0) (meoe2vec m1`At`t1) t
  ) { longitude = Long $ linearPolateT (l0`At`t0) (l1'`At`t1) t }
  where
    l0 = long $ longitude m0
    l1 = long $ longitude m1
    l1' = adjustCyclicT (l0`At`t0) (l1`At`t1) period (_2 * pi)
    period = (meoeOrbitalPeriod m0 + meoeOrbitalPeriod m1) / _2
-- -}
-- {-
linearPolateMEOEm2 :: RealFloat a
                  => Datum t a -> Datum t a
                  -> E t a -> MEOE Mean a
linearPolateMEOEm2 m0 m1 t = vec2meoe
  $ linearPolateVecT (meoe2vec <$> m0') (meoe2vec <$> m1') t
  where
    m0' = m0
    m1' = (\m -> m { longitude = Long l1' }) <$> m1
    l0 = long . longitude <$> m0
    l1 = long . longitude <$> m1
    l1' = adjustCyclicT l0 l1 period tau
    period = (meoeOrbitalPeriod (value m0) + meoeOrbitalPeriod (value m1)) / _2
-- -}

-- | Assume that y(t) is cyclic in meaning (but not in value, as
-- for e.g. angles). Then @adjustCyclic (t0,y0) (t1,y1) period cycle@
-- returns a new @y1@ adjusted so that the difference @y1 - y0@
-- corresponds roughly to the difference @t1 - t0@.
-- (See also the more general adjustCyclic.)
adjustCyclicT :: (RealFrac a, Div dy dy DOne, Mul DOne dy dy)
              => At t a (Quantity dy a) -> At t a (Quantity dy a)
              -> Time a -> Quantity dy a -> Quantity dy a
adjustCyclicT (y0`At`t0) (y1`At`t1) = adjustCyclic (f t0 ,y0) (f t1, y1)
  where f t = diffEpoch t (mjd' 0)

-- | Assume that y(x) is cyclic in meaning (but not in value, as
-- for e.g. angles). Then @adjustCyclic (x0,y0) (x1,y1) period cycle@
-- returns a new @y1@ adjusted so that the difference @y1 - y0@
-- corresponds roughly to the difference @x1 - x0@.
-- (See also adjustCyclic1.)
adjustCyclic :: (RealFrac a, Div d d DOne, Div dy dy DOne, Mul DOne dy dy)
            => (Quantity d a, Quantity dy a) -> (Quantity d a, Quantity dy a)
            -> Quantity d a -> Quantity dy a -> Quantity dy a
adjustCyclic (x0,y0) (x1,y1) period cycle =
  y1 + cyclesOff (x1-x0, y1-y0) period cycle * cycle
  -- Could be defined as:
  --   adjustCyclic1 (x0/period,y0/cycle) (x1/period,y1/cycle) * cycle
  -- but that has worse numerical properties!

-- | Adjust assuming x0 = 0 and y0 = 0
adjustCyclic0 :: (RealFrac a, Div d d DOne, Div dy dy DOne, Mul DOne dy dy)
              => (Quantity d a, Quantity dy a)
              -> Quantity d a -> Quantity dy a -> Quantity dy a
adjustCyclic0 (x1,y1) period cycle = y1 + cyclesOff (x1,y1) period cycle * cycle

-- | Assume that y(x) is cyclic in meaning (but not in value, as
-- for e.g. angles) with a periodicity @period@ and cycle length @cycle@.
-- Then @cyclesOff (x,y) period cycle@ compute the approximate (closest)
-- number of cycles that @y@ differs from what one would expect given @x@
-- an assumption that y = cycle * x / period.
cyclesOff :: (RealFrac a, Div d d DOne, Div dy dy DOne, Mul DOne dy dy)
       => (Quantity d a, Quantity dy a)
       -> Quantity d a -> Quantity dy a -> Dimensionless a
cyclesOff (x,y) period cycle = cyclesOff1 (x/period, y/cycle)

-- | Same as 'cyclesOff' but assumes that bot the period and the cycle
-- of y(x) is one.
cyclesOff1 :: RealFrac a => (Dimensionless a, Dimensionless a) -> Dimensionless a
cyclesOff1 (x, y) = fmap round' (x - y)

-- | Assume that y(x) is cyclic in meaning (but not in value, as
-- for e.g. angles) where the meaning has a cycle (in y) of 1 with
-- a period (in x) of 1. Then @adjustCyclic1 (x0,y0) (x1,y1)@
-- returns a new @y1@ adjusted so that @y1 - y0@ is roughly the same
-- as @x1 - x0@.
--
-- Property of returned y1:
--   | (y1 - y0) - (x1 - x0) | < 0.5
--
-- (This is a "normalized" version of adjustCyclic for cycle and
-- period of 1.)
adjustCyclic1 :: (RealFrac a, Ord a)
              => (Dimensionless a, Dimensionless a)
              -> (Dimensionless a, Dimensionless a)
              -> Dimensionless a
adjustCyclic1 (x0,y0) (x1,y1) = y1 + cyclesOff1 (x1-x0, y1-y0)

round' :: RealFrac a => a -> a
round' = fromIntegral . round
