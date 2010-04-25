{-
This modules provides functions for working with topocentric
coordinate systems.
-}
module Astro.Place.Topocentric where

import Astro.Place
import Vector
import Matrix
import PosVel hiding (latitude, longitude)
import Tmp.Lifts
import Tmp.ForwardAD
import ForwardAD (diffV)
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


type Axis a = Vec3 DOne DOne DOne a
type CoordSys a = Homo33 DOne a

-- | Calculates the axis of the topocentric coordinate system defined
-- by the given geodetic place. The axis definitions are those used by
-- Soop (p.222):
--   X -- towards local East,
--   Y -- towards local North,
--   Z -- towards local Zenith.
-- Note that the our Zenith is defined by the reference ellipsod (as
-- opposed to e.g. the geoid).
topocentricX, topocentricY, topocentricZ :: RealFloat a => GeodeticPlace a -> Axis a
topocentricX p = vNormalize $ diffV (\x -> geodeticToCartesian (lift p){longitude = x}) (longitude p)
topocentricY p = vNormalize $ diffV (\x -> geodeticToCartesian (lift p){latitude  = x}) (latitude  p)
topocentricZ p = vNormalize $ diffV (\x -> geodeticToCartesian (lift p){height    = x}) (height    p)

-- | Calculates the topocentric coordinate system for the given
-- geodetic place. The returned topocentric coordinate system is
-- specified in the geocentric rotating coordinate system.
topocentricCoordSys :: RealFloat a => GeodeticPlace a -> CoordSys a
topocentricCoordSys p = consRow   (topocentricX p)
                      $ consRow   (topocentricY p)
                      $ rowMatrix (topocentricZ p)

-- | Converts a position in the geocentric rotating coordinate system
-- to a position in the topocentric coordinate system defined by the
-- given geodetic place.
geocentricToTopocentric :: RealFloat a => GeodeticPlace a -> CPos a -> CPos a
geocentricToTopocentric gs sc = topocentricCoordSys gs `matVec` (sc `elemSub` geodeticToCartesian gs)

-- Causes NaNs wher the geodetic place is at the center of the ellipsoid!
topocentricToGeocentric :: RealFloat a => GeodeticPlace a -> CPos a -> CPos a
topocentricToGeocentric gs sc = transpose (topocentricCoordSys gs) `matVec` sc `elemAdd` geodeticToCartesian gs


-- | Compute elevation and azimuth in the topocentric coordinate system
-- defined by the geodetic place. The input position should be defined
-- in the geocentric coordinate system.
elevation, azimuth :: RealFloat a => GeodeticPlace a -> CPos a -> Angle a
elevation gs = declination . c2s . geocentricToTopocentric gs
azimuth   gs = azimuth'    . c2s . geocentricToTopocentric gs
  where azimuth' s = 90*~degree - rightAscension s  -- From N/Y towards E/X.

-- | Computes the range from the given geodetic place to the given
-- geocentric position.
range :: RealFloat a => GeodeticPlace a -> CPos a -> Length a
range gs = vNorm . elemSub (geodeticToCartesian gs)  -- More efficient.
--range gs = radius . c2s . geocentricToTopocentric gs  -- Rather inefficient!

-- Convert a tripple of azimuth, elevation, and range observations into
-- cartesian topocentric coordinates.
azElRgToGeocentric :: RealFloat a => GeodeticPlace a -> Angle a -> Angle a -> Length a -> CPos a
azElRgToGeocentric place az el rg = topocentricToGeocentric place $ s2c $ fromTuple (rg, 90*~degree - el, ra)
  where ra = negate az + 90*~degree


