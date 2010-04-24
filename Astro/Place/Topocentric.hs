{-
This modules provides functions for working with topocentric
coordinate systems.
-}
module Astro.Place.Topocentric (geocentricToTopocentric) where

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
-- by the given geodetic place.
topocentricX, topocentricY, topocentricZ :: RealFloat a => GeodeticPlace a -> Axis a
topocentricX p = vNormalize $ diffV (\x -> geodeticToCartesian ((lift p){longitude = x})) (longitude p) --`scaleVec'` (1*~meter)
topocentricY p = vNormalize $ diffV (\x -> geodeticToCartesian ((lift p){longitude = x})) (latitude  p) --`scaleVec'` (1*~meter)
topocentricZ p = vNormalize $ (1*~meter) `scaleVec` diffV (\x -> geodeticToCartesian ((lift p){height    = x})) (height    p)

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

topocentricAzimuth   gs = azimuth   . c2s . geocentricToTopocentric gs
topocentricElevation gs = elevation . c2s . geocentricToTopocentric gs
topocentricRange     gs = radius    . c2s . geocentricToTopocentric gs


