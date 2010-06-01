{-# LANGUAGE TypeSynonymInstances #-}

module Astro.Place.TestTopocentric where

import Numeric.Units.Dimensional (Dimensional (Dimensional))
import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Vector
import PosVel (CPos)
import Astro.Coords
import Astro.Place
import Astro.Place.Topocentric
import Astro.Place.ReferenceEllipsoid
import Astro.Util (perfectGEO)
import Test.QuickCheck
import Control.Applicative
import Data.AEq


-- Instances (TODO: move elsewhere)
-- ================================

-- Approximate equality
-- --------------------

instance AEq a => AEq (Quantity d a) where
  (Dimensional x) === (Dimensional y) = x === y
  (Dimensional x) ~== (Dimensional y) = x ~== y


instance (Floating a, AEq a) => AEq (Coord s a) where
  r1 === r2 = x1 === x2 && y1 === y2 && z1 === z2
    where
      (x1,y1,z1) = toTuple $ c r1
      (x2,y2,z2) = toTuple $ c r2
  r1 ~== r2 = x1 ~== x2 && y1 ~== y2 && z1 ~== z2
    where
      (x1,y1,z1) = toTuple $ c r1
      (x2,y2,z2) = toTuple $ c r2


-- Arbitrary instances
-- -------------------
instance (Arbitrary a) => Arbitrary (Quantity d a) where
  arbitrary   = Dimensional <$> arbitrary

instance Arbitrary a => Arbitrary (CPos a) where
    arbitrary = fromTuple <$> arbitrary

instance Arbitrary a => Arbitrary (Coord s a)
  where
    arbitrary = return . C . fromTuple =<< arbitrary

instance (Fractional a, Arbitrary a) => Arbitrary (GeodeticPlace a) where
  arbitrary = GeodeticPlace <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance (Num a, Arbitrary a) => Arbitrary (ReferenceEllipsoid a)
  where
    arbitrary = do
      x <- arbitrary
      let pol = abs x + 1*~meter  -- Prevent zero radius.
      d <- arbitrary
      let eq  = pol + abs d       -- Always larger than polar radius.
      return (ReferenceEllipsoid eq pol)


-- Helpers
-- =======

onceCheck :: (Testable prop) => prop -> IO ()
onceCheck = quickCheckWith stdArgs { maxSuccess = 1 }
manyCheck :: (Testable prop) => prop -> IO ()
manyCheck = quickCheckWith stdArgs { maxSuccess = 1000, maxDiscard = 1000 }

-- | Comparison allowing for specified inaccuracy.
cmpE :: (Fractional a, Ord a) => Quantity d a -> Quantity d a -> Quantity d a -> Bool
cmpE accuracy x y' = abs (x - y') < accuracy


-- Properties
-- ==========

-- | 3rd party data validation.
prop_va long f x = cmpE e (f cibinong $ C $ perfectGEO $ long*~degree) (x*~degree)
  where
    e = 0.1 *~ degree
    cibinong = GeodeticPlace wgs84
      (negate $ 6*~degree + 26*~arcminute + 52*~arcsecond)
      (106*~degree + 56*~arcminute + 10*~arcsecond)
      (0*~meter)

-- | Converting geocentric to topocentric and back should be identity function.
prop_topo1 :: GeodeticPlace Double -> Coord ECR Double -> Property
prop_topo1 place p = not (degeneratePlace place) ==> p ~== p'
  where
    p' = topocentricToGeocentric place $ geocentricToTopocentric place $ p

-- | Converting topocentric to geocentric and back should be identity function.
prop_topo2 :: GeodeticPlace Double -> Coord Topocentric Double -> Property
prop_topo2 place p = not (degeneratePlace place) ==> p ~== p'
  where
    p' = geocentricToTopocentric place $ topocentricToGeocentric place $ p

-- | Going to and from az/el/rg observations is id.
prop_obs :: GeodeticPlace Double -> Coord ECR Double -> Property
prop_obs place p = not (degeneratePlace place) ==> p ~== p'
  where
    az = azimuth   place p
    el = elevation place p
    rg = range     place p
    p' = topocentricToGeocentric place $ azElRgToCoords az el rg


-- Driver
-- ======

main = do
  onceCheck $ prop_va 108   azimuth    9.3
  onceCheck $ prop_va 108   elevation 82.3 -- TODO 82.0 according to VA...?
  onceCheck $ prop_va 118   azimuth   60.1
  onceCheck $ prop_va 118   elevation 75.0
  onceCheck $ prop_va 142   azimuth   80.9
  onceCheck $ prop_va 142   elevation 48.7
  onceCheck $ prop_va 150.5 azimuth   83.3
  onceCheck $ prop_va 150.5 elevation 39.3
  onceCheck $ prop_va 172   azimuth   87.0
  onceCheck $ prop_va 172   elevation 16.4
  onceCheck $ prop_degenerate1
  manyCheck prop_topo1
  onceCheck $ prop_topo1_fail1
  manyCheck prop_topo2
  manyCheck prop_obs
  onceCheck $ prop_obs_fail1
  onceCheck $ prop_obs_fail2
  onceCheck $ prop_obs_fail3


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

prop_obs_fail1 = prop_obs place p
  where
    p = C $ fromTuple ((-16.424578194638578)*~meter
                      , (-89.47810248383104)*~meter
                      , (-1566.516520378069)*~meter)
    place = GeodeticPlace (ReferenceEllipsoid (136607.51358690596*~meter)
                                                (155.740741518507*~meter))
                          (79.93674030440873*~radian)
                          (60.60648347976257*~radian)
                          (58.278163555009634*~meter)

prop_obs_fail2 = prop_obs place p
  where
    p = C $ fromTuple (95.76534753065962*~meter
                  , (-26.02673518931036)*~meter
                  , (-75.65015287140265)*~meter)
    place = GeodeticPlace (ReferenceEllipsoid (276733.3457887228*~meter)
                                              (276730.57152002316*~meter))
                          ((-152.91564055491563)*~radian)
                          ((-94.14913720744624)*~radian)
                          (187.078596831506*~meter)

prop_obs_fail3 = prop_obs place p
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
