{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Astro.Orbit.MEOE where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (revolution, gee)
import Numeric.Units.Dimensional.LinearAlgebra
import Astro.Orbit.Types
import qualified Prelude

-- | Modified Equinoctial Orbital Elements as defined by Walker et al.
data MEOE long a = MEOE
  { mu :: GravitationalParameter a
  , p  :: Length a  -- Also known as semi latus rectum.
  , f  :: Dimensionless a
  , g  :: Dimensionless a
  , h  :: Dimensionless a
  , k  :: Dimensionless a
  , longitude :: Longitude long a
  -- ^ Longitude in the sense of RAAN + AoP + Anomaly, not in the
  -- geographic sense. See Eagle. It is equal to Right Ascension
  -- for an orbit with inclinaton = 0°.
  } deriving (Show)

-- | Convert a MEOE into a vector (not a State Vector) of its
-- elements (including mu).
meoe2vec :: Fractional a => MEOE m a -> Vec DOne 7 a
meoe2vec MEOE{..} = (mu / mu_Earth) <: (p / r_GEO) <: f <: g <: h <: k <:. long longitude
  where
    -- TODO substitute the below?
    mu_Earth = 398600.4418 *~ (kilo meter ^ pos3 / second ^ pos2)
    r_GEO    = 42164.2 *~ kilo meter

vec2meoe :: Fractional a => Vec DOne 7 a -> MEOE m a
vec2meoe v = MEOE (mu' * mu_Earth) (p' * r_GEO) f g h j (Long lon)
  where
    [mu', p', f, g, h, j, lon] = toList v
    -- TODO substitute the below?
    mu_Earth = 398600.4418 *~ (kilo meter ^ pos3 / second ^ pos2)
    r_GEO    = 42164.2 *~ kilo meter


-- TODO move orbitalPeriod somewhere more generic?
orbitalPeriod :: Floating a => GravitationalParameter a -> SemiMajorAxis a -> Time a
orbitalPeriod mu (SMA a) = tau * sqrt (a ^ pos3 / mu)

-- | Compute the orbital eccentricity.
eccentricity :: Floating a => MEOE t a -> Dimensionless a
eccentricity = sqrt . eccentricity2

-- | Orbital eccentricity squared. This is a useful quantity as it is used in
-- some other formulae and is a simpler computation than eccentricity
-- itself.
eccentricity2 :: Fractional a => MEOE t a -> Dimensionless a
eccentricity2 MEOE{..} = f ^ pos2 + g ^ pos2

-- | The semi latus rectum is the @p@ element of a MEOE
semiLatusRectum :: Fractional a => MEOE t a -> Length a
semiLatusRectum = p

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


-- --------------------------------
-- Perturbations
-- --------------------------------

-- | The mean rate of the MEOE longitude, not taking into account
-- variations in rate due to eccentricity.
meanLongitudeRate :: Floating a => MEOE t a -> AngularVelocity a
meanLongitudeRate m = 1 *~ revolution / meoeOrbitalPeriod m
--meanLongitudeRate m = tau / meoeOrbitalPeriod m

-- | The instantaneous rate of change of the MEOE longitude due to
-- keplerian motion around the central body only.
longitudeRate :: Floating a => MEOE True a -> AngularVelocity a
longitudeRate MEOE{..} = sqrt (mu * p) * (w / p) ^ pos2
  where
    l = long longitude
    w = _1 + f * cos l + g * sin l

-- | Apply an impulsive perturbation (delta-velocity) specified in TNR
-- coordinates to the MEOE. See Eagle.
impulsivePerturbation :: RealFloat a => MEOE True a -> Velocity a -> Velocity a -> Velocity a -> MEOE True a
impulsivePerturbation MEOE{..} dvr dvt dvn = MEOE
  { mu = mu
  , p = p + dp
  , f = f + df
  , g = g + dg
  , h = h + dh
  , k = k + dk
  , longitude = Long $ l + dl
  } where
    -- Helpers
    l = long longitude
    s2 = _1 + h ^ pos2 + k ^ pos2
    w = _1 + f * cos l + g * sin l
    sqpmu = sqrt (p / mu)
    -- Element changes
    dp = sqpmu * _2 * p / w * dvt
    df = sqpmu * ( sin l  * dvr
                 + ((w + _1) * cos l + f) / w * dvt
                 - (h * sin l - k * cos l) * g / w * dvn
                 )
    dg = sqpmu * ( negate (cos l) * dvr
                 + ((w + _1) * sin l + g) / w * dvt
                 + (h * sin l - k * cos l) * g / w * dvn
                 )
    dh = sqpmu * s2 / (_2 * w) * cos l * dvn
    dk = sqpmu * s2 / (_2 * w) * sin l * dvn
    dl = sqpmu / w * (h * sin l - k * cos l) * dvn
