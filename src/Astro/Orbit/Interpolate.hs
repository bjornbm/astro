{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Astro.Orbit.Interpolate where

import qualified Prelude
import Control.Applicative

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra

import Astro.Orbit.Types
import Astro.Time
import Astro.Time.At
import Astro.Util.Cyclic
import Astro.Orbit.MEOE
import Astro.Trajectory (Datum)


-- dimensional (TODO: move?)
-- -------------------------
-- Interpolate.
linearPolate :: Fractional a -- (Div d d DOne, Mul DOne dy dy, Fractional a)
             => (Quantity d a, Quantity dy a) -> (Quantity d a, Quantity dy a)
             -> Quantity d a -> Quantity dy a
linearPolate (x0,y0) (x1,y1) x = linearPolate0 y0 (x1-x0,y1) (x-x0)

-- Polate assuming y0 corresponds to x0 = 0.
linearPolate0 :: Fractional a -- (Div d d DOne, Mul DOne dy dy, Fractional a)
              => Quantity dy a -> (Quantity d a, Quantity dy a)
              -> Quantity d a -> Quantity dy a
--linearPolate0 y0 (x1,y1) x = y0 + x / x1 * (y1 - y0)
linearPolate0 y0 (x1,y1) x = y0 + linearPolate00 (x1,y1-y0) x

-- Polate assuming x0 = 0 and y0 = 0.
linearPolate00 :: Fractional a -- (Div d d DOne, Mul DOne dy dy, Fractional a)
               => (Quantity d a, Quantity dy a)
               -> Quantity d a -> Quantity dy a
linearPolate00 (x1,y1) x = x / x1 * y1


-- dimensional-vectors (TODO: move?)
-- ---------------------------------
-- Interpolate vector.
linearPolateVec :: Fractional a -- (Div d d DOne, Fractional a)
                => (Quantity d a, Vec ds n a) -> (Quantity d a, Vec ds n a)
                -> Quantity d a -> Vec ds n a
linearPolateVec (x0,v0) (x1,v1) x = linearPolateVec0 v0 (x1-x0,v1) (x-x0)

-- Polate assuming v0 corresponds to x0 = 0.
linearPolateVec0 :: Fractional a -- (Div d d DOne, Fractional a)
                 => Vec ds n a -> (Quantity d a, Vec ds n a)
                 -> Quantity d a -> Vec ds n a
--linearPolateVec0 v0 (x1,v1) x = v0 >+< scaleVec1 (x / x1) (v1 >-< v0)
linearPolateVec0 v0 (x1,v1) x = v0 >+< linearPolateVec00 (x1, v1 >-< v0) x

-- Polate assuming x0 = 0 and v0 = {0}.
linearPolateVec00 :: Fractional a -- (Div d d DOne, Fractional a)
                  => (Quantity d a, Vec ds n a)
                  -> Quantity d a -> Vec ds n a
linearPolateVec00 (x1,v1) x = scaleVec (x / x1) v1


-- Depend on astro
-- ---------------
-- | Interpolate or extrapolate linearly.
linearPolateT :: Fractional a -- (Mul DOne d d, Fractional a)
              => At t a (Quantity d a) -> At t a (Quantity d a)
              -> E t a -> Quantity d a
linearPolateT (x0`At`t0) (x1`At`t1) t = linearPolate0 x0 (d t1,x1) (d t)
  where d = (`diffEpoch` t0)

-- Interpolate vector as function of time.
linearPolateVecT :: Fractional a
                 => At t a (Vec ds n a) -> At t a (Vec ds n a) -> E t a -> Vec ds n a
linearPolateVecT (v0`At`t0) (v1`At`t1) t = linearPolateVec0 v0 (d t1,v1) (d t)
  where d = (`diffEpoch` t0)

-- ==================================================================

-- | Interpolate two MEOEs with mean anomaly. In interpolating mean
-- anomaly the orbital period is taken into account to ensure proper
-- interpolation.
-- NOTE: this does not work for hyperbolic orbits, and probably not
-- when t1 and t0 are separated by more than one (or a half?) orbital
-- period?
linearPolateMEOEm :: RealFloat a => Datum t a -> Datum t a
                  -> E t a -> MEOE Mean a
linearPolateMEOEm m0 m1 t = vec2meoe
  $ linearPolateVecT (meoe2vec <$> m0) (meoe2vec <$> m1') t
  where
    m1' = (\m -> m { longitude = Long l1' }) <$> m1
    l0 = long . longitude <$> m0
    l1 = long . longitude <$> m1
    l1' = adjustCyclicT l0 l1 period tau
    period = (meoeOrbitalPeriod (value m0) + meoeOrbitalPeriod (value m1)) / _2

-- {-
linearPolateMEOEm_OLD :: RealFloat a
                  => Datum t a -> Datum t a
                  -> E t a -> MEOE Mean a
linearPolateMEOEm_OLD (m0`At`t0) (m1`At`t1) t = ( vec2meoe
  $ linearPolateVecT (meoe2vec m0`At`t0) (meoe2vec m1`At`t1) t
  ) { longitude = Long $ linearPolateT (l0`At`t0) (l1'`At`t1) t }
  where
    l0 = long $ longitude m0
    l1 = long $ longitude m1
    l1' = adjustCyclicT (l0`At`t0) (l1`At`t1) period tau
    period = (meoeOrbitalPeriod m0 + meoeOrbitalPeriod m1) / _2
-- -}
