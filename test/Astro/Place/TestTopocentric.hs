{-# LANGUAGE TypeSynonymInstances #-}

module Astro.Place.TestTopocentric where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude
import Astro.Coords
import Astro.Place
import Astro.Place.Topocentric
import Astro.Place.ReferenceEllipsoid
import Astro.Util (perfectGEO, sexagesimalAngle)
import Test.QuickCheck
import TestUtil
import TestInstances
import Data.AEq


-- Properties
-- ==========

-- | 3rd party data validation.
prop_va long f x = cmpE e (f cibinong $ perfectGEO $ long*~degree) (x*~degree)
  where
    e = 0.1 *~ degree
    cibinong = GeodeticPlace wgs84
      (negate $ sexagesimalAngle 6 26 52)
      (sexagesimalAngle 106 56 10)
      (0*~meter)

-- | Converting geocentric to topocentric and back should be identity function.
prop_topo1 :: GeodeticPlace Double -> Coord ECR Double -> Property
prop_topo1 place p = not (degeneratePlace place) ==> p ~== p'
  where
    p' = topocentricToECR place $ ecrToTopocentric place $ p
{- prop_topo1
*** Failed! Falsifiable (after 720 tests):
GeodeticPlace {refEllips = ReferenceEllipsoid {equatorialRadius = 21972.493449352645 m, polarRadius = 2.1731050756815637 m}, latitude = 39.30500938162273, longitude = 69.62167787937885, height = 26.50133090234193 m}
C < -12.02345499632946 m, -138.2024450210176 m, -23.342674229564704 m >
-}

-- | Converting topocentric to geocentric and back should be identity function.
prop_topo2 :: GeodeticPlace Double -> Coord Topocentric Double -> Property
prop_topo2 place p = not (degeneratePlace place) ==> p ~== p'
  where
    p' = ecrToTopocentric place $ topocentricToECR place $ p

-- | Going to and from az/el/rg observations is id.
prop_obs :: Coord Topocentric Double -> Bool
prop_obs p = p ~== p'
  where
    az = azimuth   p
    el = elevation p
    rg = range     p
    p' = azElRgToCoords az el rg

-- | Going to and from az/el/rg observations in ECR is id.
prop_obsECR :: GeodeticPlace Double -> Coord ECR Double -> Property
prop_obsECR place p = not (degeneratePlace place) ==> p ~== p'
  where
    az = azimuth'   place p
    el = elevation' place p
    rg = range'     place p
    p' = topocentricToECR place $ azElRgToCoords az el rg


-- Driver
-- ======

main = do
  onceCheck $ prop_va 108   azimuth'    9.3
  onceCheck $ prop_va 108   elevation' 82.3 -- TODO 82.0 according to VA...?
  onceCheck $ prop_va 118   azimuth'   60.1
  onceCheck $ prop_va 118   elevation' 75.0
  onceCheck $ prop_va 142   azimuth'   80.9
  onceCheck $ prop_va 142   elevation' 48.7
  onceCheck $ prop_va 150.5 azimuth'   83.3
  onceCheck $ prop_va 150.5 elevation' 39.3
  onceCheck $ prop_va 172   azimuth'   87.0
  onceCheck $ prop_va 172   elevation' 16.4
  onceCheck $ prop_degenerate1
  manyCheck prop_topo1
  onceCheck $ prop_topo1_fail1
  manyCheck prop_topo2
  manyCheck prop_obs
  manyCheck prop_obsECR
  onceCheck $ prop_obsECR_fail1
  onceCheck $ prop_obsECR_fail2
  onceCheck $ prop_obsECR_fail3


-- Prior failures
-- ==============

-- A degenerate place (see Astro.Place) was causing a test failure.
-- Now test inputs are checked for degeneracy. This property verifies
-- that the degenerary test catches the original problematic case.
prop_degenerate1 = degeneratePlace place
  where
    place = GeodeticPlace (ReferenceEllipsoid (4.0*~meter) (3.0*~meter))
                          _0 _0 ((-2.25)*~meter)


-- FP inaccuracies
-- ---------------
-- The below tests failed because poor geometries (e.g., flat ellipsoids)
-- cause FP errors to grow. Switching to using Data.AEq from ieee package
-- appears to have rectified this.

prop_topo1_fail1 = prop_topo1 place p
  where
    p = C $ fromTuple ((-240.34114081130468)*~meter
                        , 102.79932480283043*~meter
                        , 186.62064762372555*~meter)
    place = GeodeticPlace (ReferenceEllipsoid (136781.86798882156*~meter)
                                              (167.72821654743436*~meter))
                          ((-2243.8289959047747)*~radian)
                          (20.39182594235854*~radian)
                          (13.25536345487733*~meter)

prop_obsECR_fail1 = prop_obsECR place p
  where
    p = C $ fromTuple ((-16.424578194638578)*~meter
                      , (-89.47810248383104)*~meter
                      , (-1566.516520378069)*~meter)
    place = GeodeticPlace (ReferenceEllipsoid (136607.51358690596*~meter)
                                                (155.740741518507*~meter))
                          (79.93674030440873*~radian)
                          (60.60648347976257*~radian)
                          (58.278163555009634*~meter)
{- prop_obsECR
*** Failed! Falsifiable (after 746 tests):
GeodeticPlace {refEllips = ReferenceEllipsoid {equatorialRadius = 311179.0845705706 m, polarRadius = 4.703271503709908 m}, latitude = 82.75891566042836, longitude = -97.08922573273038, height = 34.00110203891187 m}
C < 10.924475023284334 m, -113.26486229757461 m, 43.599269604222506 m >
-}


prop_obsECR_fail2 = prop_obsECR place p
  where
    p = C $ fromTuple (95.76534753065962*~meter
                  , (-26.02673518931036)*~meter
                  , (-75.65015287140265)*~meter)
    place = GeodeticPlace (ReferenceEllipsoid (276733.3457887228*~meter)
                                              (276730.57152002316*~meter))
                          ((-152.91564055491563)*~radian)
                          ((-94.14913720744624)*~radian)
                          (187.078596831506*~meter)

prop_obsECR_fail3 = prop_obsECR place p
  where
    p = C $ fromTuple ((-12.670478806591223)*~meter
                      , (-7.520050866897443)*~meter
                      , (-67.76922633914856)*~meter )
    place = GeodeticPlace
          {refEllips = ReferenceEllipsoid
                     {equatorialRadius = 134.15569877277596*~meter
                     , polarRadius = 65.24417626981652*~meter}
          , latitude = 96.5645385211402*~radian
          , longitude = (-5.66974812882677)*~radian
          , height = 174062.2962703866*~meter}

{-
*** Failed! Falsifiable (after 903 tests):  
GeodeticPlace {refEllips = ReferenceEllipsoid {equatorialRadius = 154314.88808559094 m, polarRadius = 92.02164361899281 m}, latitude = 64.84504602560372, longitude = -12.454605679732511, height = 59.292381438053056 m}
C < 16.99978234375992 m, -60.30380096338773 m, 4.689191066151387 m >
-}
