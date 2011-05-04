{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Astro.Orbit.MEOE where

import Numeric.NumType (incr)
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Astro.Orbit.Types
import qualified Prelude

-- | Modified Equinoctial Orbital Elements as defined by Walker et al.
data MEOE long a = MEOE
  { mu :: GravitationalParameter a
  , p  :: Length a
  , f  :: Dimensionless a
  , g  :: Dimensionless a
  , h  :: Dimensionless a
  , k  :: Dimensionless a
  , longitude :: Longitude long a
  } deriving (Show)

-- | Convert a MEOE into a vector (not a State Vector) of its
-- elements (including mu).
meoe2vec MEOE{..} = mu <: p <: f <: g <: h <: k <:. long longitude

vec2meoe v = MEOE (vElemAt zero v)
                  (vElemAt pos1 v)
                  (vElemAt pos2 v)
                  (vElemAt pos3 v)
                  (vElemAt pos4 v)
                  (vElemAt pos5 v)
                  (Long $ vElemAt (incr pos5) v)



orbitalPeriod :: Floating a => GravitationalParameter a -> SemiMajorAxis a -> Time a
orbitalPeriod mu (SMA a) = _2 * pi * sqrt (a ^ pos3 / mu)

eccentricity2 :: Fractional a => MEOE t a -> Dimensionless a
eccentricity2 MEOE{..} = f ^ pos2 + g ^ pos2

semiMajorAxis :: Fractional a => MEOE t a -> SemiMajorAxis a
semiMajorAxis m = SMA $ p m / (_1 - eccentricity2 m)

meoeOrbitalPeriod :: Floating a => MEOE t a -> Time a
meoeOrbitalPeriod meoe = orbitalPeriod (mu meoe) (semiMajorAxis meoe)
