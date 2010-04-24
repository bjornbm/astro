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

-- | Compute elevation and azimuth in the topocentric coordinate system
-- defined by the geodetic place.
elevation, azimuth :: RealFloat a => GeodeticPlace a -> CPos a -> Angle a
elevation gs = declination . c2s . geocentricToTopocentric gs
azimuth   gs = azimuth'    . c2s . geocentricToTopocentric gs
  where azimuth' s = 90*~degree - rightAscension s
--range gs = radius . c2s . geocentricToTopocentric gs  -- Rather inefficient!
--range gs = vNorm . elemSub (geodeticToCartesian gs)  -- More efficient.


