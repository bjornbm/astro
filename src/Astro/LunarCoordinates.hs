{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Astro.LunarCoordinates (lunarCoordinates) where

import Astro.Time
import Astro.Coords
import Astrodynamics (r_GEO)
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.PosVel (CPos, Sph (Sph), toZenith)
import Numeric.Units.Dimensional.NonSI (century)
import qualified Prelude

{- |
This module implements the "Low-precision formulae for geocentric
coordinates of the Moon" from page D22 of The Astronomical Almanac
(2009).

According to the almanac (2018), during the period 1900 to 2100
the errors will rarely exceed
0.3° in ecliptic longitude,
0.2° in ecliptic latitude,
0.003° in horizontal parallax,
0.001° in semidiameter,
0.2 Earth radii in distance,
0.3° in right ascension
and 0.2° in declination.

The coordinate system of the computation result is not well
specified. However, with the given accuracy any Earth-centered
inertial coordinate system should do.
-}

-- | Computes the Moon's coordinates to an accuracy of about 0.3°
-- during the period of 1900 to 2100.
lunarCoordinates :: forall a . RealFloat a => E UT1 a -> Coord GCRS a
lunarCoordinates ut1 = C (x <: y <:. z)
  where
    lambda = lunarEclipticLongitude ut1
    beta = lunarEclipticLatitude ut1
    r = lunarDistance0 ut1

    -- Geocentric direction cosines (l, m, n).
    cosine_l = cos beta / cos lambda  -- cos decl * cos ra
    cosine_m = 0.9175 *~ one * cos beta * sin lambda - 0.3978 *~ one * sin beta  -- = cos decl * sin ra
    cosine_n = 0.3978 *~ one * cos beta * sin lambda + 0.9175 *~ one * sin beta  -- = sin decl

    -- Geocentric rectangular coordinates
    x, y, z :: Length a
    x = r * cosine_l  -- = r * cos decl * cos ra
    y = r * cosine_m  -- = r * cos decl * sin ra
    z = r * cosine_n  -- = r * sin decl

    {-
    -- Right ascension and declination are referred to the mean equator and equinox of date.
    rightAscension_alpha = atan2 cosine_m cosine_l :: PlaneAngle a
    declination_delta = asin cosine_n :: PlaneAngle a
    spherical = S $ Sph r (toZenith declination_delta) rightAscension_alpha
    -}


lunarEclipticLongitude :: RealFloat a => E UT1 a -> PlaneAngle a
lunarEclipticLongitude ut1 = let t = sinceJ2000' ut1 in
  deg 218.32 + dpc 481_267.881 * t
  + deg 6.29 * sin (deg 135.0 + dpc 477_198.87 * t) - deg 1.27 * sin (deg 259.3 - dpc 413_335.36 * t)
  + deg 0.66 * sin (deg 235.7 + dpc 890_534.22 * t) + deg 0.21 * sin (deg 269.9 + dpc 954_397.74 * t)
  - deg 0.19 * sin (deg 357.5 + dpc 035_999.05 * t) - deg 0.11 * sin (deg 186.5 + dpc 966_404.03 * t)


lunarEclipticLatitude :: RealFloat a => E UT1 a -> PlaneAngle a
lunarEclipticLatitude ut1 = let t = sinceJ2000' ut1 in _0
  + deg 5.13 * sin (deg 093.3 + dpc 483_202.02 * t) + deg 0.28 * sin (deg 228.2 + dpc 960_400.89 * t)
  - deg 0.28 * sin (deg 235.7 + dpc 006_002.15 * t) - deg 0.17 * sin (deg 217.6 - dpc 407_332.21 * t)


-- | The horizontal parallax of the Moon as seen from the center of Earth.
lunarHorizontalParallax0 :: RealFloat a => E UT1 a -> PlaneAngle a
lunarHorizontalParallax0 ut1 = let t = sinceJ2000' ut1 in _0
  + deg 0.9508
  + deg 0.0518 * cos (deg 135.0 + dpc 477_198.87 * t) + deg 0.0095 * cos (deg 259.3 - dpc 413_335.36 * t)
  + deg 0.0078 * cos (deg 235.7 + dpc 890_534.22 * t) + deg 0.0028 * cos (deg 269.9 + dpc 954_397.74 * t)

-- | The semidiameter (SD) of the Moon as seen from the center of Earth.
lunarSemidiameter0 :: RealFloat a => E UT1 a -> PlaneAngle a
lunarSemidiameter0 ut1 = 0.0274 *~ one * lunarHorizontalParallax0 ut1

-- | The distance to the Moon from the center of Earth.
lunarDistance0 :: RealFloat a => E UT1 a -> Length a
lunarDistance0 ut1 = r_GEO / sin (lunarHorizontalParallax0 ut1)


-- | Convenience function for angles in degrees.
deg :: Floating a => a -> PlaneAngle a
deg x = x *~ degree

-- | Convenience function for angular velocity in degrees per Julian century.
dpc :: Floating a => a -> AngularVelocity a
dpc x = x *~ (degree / century)

{-
-- | The horizontal parallax of the Moon as seen from the given coordinates.
lunarHorizontalParallax :: RealFloat a => Coord GCRS a -> E UT1 a -> PlaneAngle a
lunarHorizontalParallax xyz ut1 = undefined

-- | The semidiameter (SD) of the Moon as seen from the given coordinates.
lunarSemidiameter :: forall a . Coord GCRS a -> E UT1 a -> PlaneAngle a
lunarSemidiameter xyz ut1 = undefined
-}
