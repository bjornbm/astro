{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpolate where

-- import Web.AstroPosVel (Datum)
-- import Synchronized
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
import qualified Prelude as P

type Datum a = (E UT1 a, PosVel MEGSD a)


-- dimensional
-- -----------
-- Interpolate.
linearPolate :: (Div d d DOne, Mul DOne dy dy, Fractional a)
             => (Quantity d a, Quantity dy a) -> (Quantity d a, Quantity dy a)
             -> Quantity d a -> Quantity dy a
linearPolate (x0,y0) (x1,y1) x = y0 + ((x - x0) / (x1 - x0)) * (y1 - y0)


-- dimensional-vectors
-- -------------------
-- Interpolate vector.
linearPolateVec :: (Div d d DOne, Fractional a)
                => (Quantity d a, Vec ds a) -> (Quantity d a, Vec ds a)
                -> Quantity d a -> Vec ds a
linearPolateVec (x0,v0) (x1,v1) x = v0 `elemAdd` scaleVec1 ((x - x0) / (x1 - x0)) (v1 `elemSub` v0)


-- Depend on astro
-- ---------------

-- | Interpolate or extrapolate linearly.
linearPolateT :: (Mul DOne d d, Fractional a)
              => (E t a, Quantity d a) -> (E t a, Quantity d a)
              -> E t a -> Quantity d a
linearPolateT (t1, x1) (t2, x2) t = x1 + diffEpoch t t1 / diffEpoch t2 t1 * (x2 - x1)

-- Interpolate vector as function of time.
linearPolateVecT :: Fractional a
                 => (E t a, Vec ds a) -> (E t a, Vec ds a) -> E t a -> Vec ds a
linearPolateVecT (t1,v1) (t2,v2) t = linearPolateVec (f t1,v1) (f t2,v2) (f t)
  where f t = diffEpoch t (mjd' 0)


-- ==================================================================

-- | Interpolate two MEOEs with mean anomaly. In interpolating mean
-- anomaly the orbital period is taken into account to ensure proper
-- interpolation.
linearPolateMEOEm :: RealFloat a
                  => (E t a, MEOE Mean a) -> (E t a, MEOE Mean a)
                  -> E t a -> MEOE Mean a
linearPolateMEOEm tm0@(t0,m0) tm1@(t1,m1) t = ( vec2meoe
  $ linearPolateVecT (fmap meoe2vec tm0) (fmap meoe2vec tm1) t
  ) { longitude = Long $ linearPolateT (t0,l0) (t1,l1') t }
  where
    l0 = long $ longitude m0
    l1 = long $ longitude m1
    l1' = adjustCyclicT (t0,l0) (t1,l1) period (_2 * pi)
    period = linearPolateT (fmap meoeOrbitalPeriod tm0) (fmap meoeOrbitalPeriod tm1) t


-- | Assume that y(t) is cyclic in meaning (but not in value, as
-- for e.g. angles). Then @adjustCyclic (t0,y0) (t1,y1) period cycle@
-- returns a new @y1@ adjusted so that the difference @y1 - y0@
-- corresponds roughly to the difference @t1 - t0@.
-- (See also the more general adjustCyclic.)
adjustCyclicT :: (RealFrac a, Div dy dy DOne, Mul DOne dy dy)
              => (E t a, Quantity dy a) -> (E t a, Quantity dy a)
              -> Time a -> Quantity dy a -> Quantity dy a
adjustCyclicT (t0,y0) (t1,y1) = adjustCyclic (f t0 ,y0) (f t1, y1)
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
  adjustCyclic1 (x0/period,y0/cycle) (x1/period,y1/cycle) * cycle


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
adjustCyclic1 (x0,y0) (x1,y1)
  = y1 + (fromIntegral $ round $ ((x1 - x0) - (y1 - y0)) /~ one) *~ one
