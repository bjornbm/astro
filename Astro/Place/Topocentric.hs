{-# OPTIONS_GHC -fcontext-stack=22 #-}

{-
This modules provides functions for working with topocentric
coordinate systems.
-}
module Astro.Place.Topocentric where

import Astro.Coords
import Astro.Place
import PosVel hiding (latitude, longitude)
import AD
import Tmp.Lifts
import VectorAD (diffV)
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude


-- | Calculates the axis of the topocentric coordinate system defined
-- by the given geodetic place. The axis definitions are those used by
-- Soop (p.222):
--   X -- towards local East,
--   Y -- towards local North,
--   Z -- towards local Zenith.
-- Note that the our Zenith is defined by the reference ellipsod (as
-- opposed to e.g. the geoid).
topocentricX, topocentricY, topocentricZ :: RealFloat a => GeodeticPlace a -> Axis a
topocentricX p = vNormalize $ diffV (\x -> c $ geodeticToECR (lift p){longitude = x}) (longitude p)
topocentricY p = vNormalize $ diffV (\x -> c $ geodeticToECR (lift p){latitude  = x}) (latitude  p)
topocentricZ p = vNormalize $ diffV (\x -> c $ geodeticToECR (lift p){height    = x}) (height    p)

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
ecrToTopocentric :: RealFloat a => GeodeticPlace a -> Coord ECR a -> Coord Topocentric a
ecrToTopocentric gs sc = C $ topocentricCoordSys gs `matVec` c (diffCoords sc (geodeticToECR gs))

-- Causes NaNs when the geodetic place is at the center of the ellipsoid!
topocentricToECR :: RealFloat a => GeodeticPlace a -> Coord Topocentric a -> Coord ECR a
topocentricToECR gs sc = C $ (transpose (topocentricCoordSys gs) `matVec` c sc) `elemAdd` c (geodeticToECR gs)


elevation :: RealFloat a => Coord Topocentric a -> Angle a
elevation = declination . s

azimuth :: RealFloat a => Coord Topocentric a -> Angle a
azimuth = az . s
  where az v = 90*~degree - rightAscension v  -- From N/Y towards E/X.

range :: RealFloat a => Coord Topocentric a -> Length a
range = radius . s

-- | Compute elevation in the topocentric coordinate system
-- defined by the geodetic place. The input position should be defined
-- in the geocentric coordinate system.
elevation' :: RealFloat a => GeodeticPlace a -> Coord ECR a -> Angle a
elevation' gs = declination . s . ecrToTopocentric gs

-- | Compute azimuth in the topocentric coordinate system
-- defined by the geodetic place. The input position should be defined
-- in the geocentric coordinate system.
azimuth' :: RealFloat a => GeodeticPlace a -> Coord ECR a -> Angle a
azimuth'   gs = azimuth' . s . ecrToTopocentric gs
  where azimuth' s = 90*~degree - rightAscension s  -- From N/Y towards E/X.

-- | Computes the range from the given geodetic place to the given
-- geocentric position.
range' :: RealFloat a => GeodeticPlace a -> Coord ECR a -> Length a
range' gs = vNorm . c . diffCoords (geodeticToECR gs)  -- More efficient.
--range gs = radius . s . ecrToTopocentric gs  -- Rather inefficient!

-- Convert a tripple of azimuth, elevation, and range observations into
-- cartesian coordinates in the topocentric system of the measurement
-- source.
azElRgToCoords :: RealFloat a => Angle a -> Angle a -> Length a -> Coord Topocentric a
azElRgToCoords az el rg = S $ fromTuple (rg, 90*~degree - el, negate az + 90*~degree)
