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
              -- ^ Longitude in the sense of RAAN + AoP + Anomaly,
              -- not in the geographic sense.
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



-- TODO move orbitalPeriod somewhere more generic?
orbitalPeriod :: Floating a => GravitationalParameter a -> SemiMajorAxis a -> Time a
orbitalPeriod mu (SMA a) = _2 * pi * sqrt (a ^ pos3 / mu)

-- | Compute the orbital eccentricity.
eccentricity :: Floating a => MEOE t a -> Dimensionless a
eccentricity = sqrt . eccentricity2

-- | Orbital eccentricity squared. This is a useful quantity as it is used in
-- some other formulae and is a simpler computation than eccentricity
-- itself.
eccentricity2 :: Fractional a => MEOE t a -> Dimensionless a
eccentricity2 MEOE{..} = f ^ pos2 + g ^ pos2

semiMajorAxis :: Fractional a => MEOE t a -> SemiMajorAxis a
semiMajorAxis m = SMA $ p m / (_1 - eccentricity2 m)

meoeOrbitalPeriod :: Floating a => MEOE t a -> Time a
meoeOrbitalPeriod meoe = orbitalPeriod (mu meoe) (semiMajorAxis meoe)

-- | Compute the orbital inclination.
inclination :: RealFloat a => MEOE t a -> Angle a
inclination MEOE{..} = _2 * atan (sqrt (h^pos2 + k^pos2))

-- | Argument of Periapsis is the angle from the ascending node to the
-- periapsis (e.g. perigee) of the orbit.
argumentOfPeriapsis :: RealFloat a => MEOE long a -> Angle a
argumentOfPeriapsis MEOE{..} = atan2 (g * h - f * k) (f * h + g * k)

-- | Right Ascension of the Ascending Node.
raan :: RealFloat a => MEOE long a -> Angle a
raan MEOE{..} = atan2 k h

-- | Anomaly is the angle between periapsis and the satellite. True or
-- mean depending on the @MEOE@.
anomaly :: RealFloat a => MEOE t a -> Anomaly t a
anomaly m = Anom $ long (longitude m) - (raan m + argumentOfPeriapsis m)
--anomaly MEOE{..} = Anom $ long longitude - atan2 g f

-- | Argument of latitude is the angle from the ascending node to the
-- satellite.
argumentOfLatitude :: RealFloat a => MEOE True a -> Angle a
argumentOfLatitude MEOE{..} = let l = long longitude in
                              atan2 (h * sin l - k * cos l)
                                    (h * cos l + k * sin l)
