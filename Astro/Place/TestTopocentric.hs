{-# LANGUAGE TypeSynonymInstances #-}

module Astro.Place.TestTopocentric where

import Numeric.Units.Dimensional (Dimensional (Dimensional))
import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Vector
import PosVel (CPos)
import Astro.Place
import Astro.Place.Topocentric
import Astro.Place.ReferenceEllipsoid
import Astro.Util (perfectGEO)
import Test.QuickCheck
import Control.Applicative


-- Preliminaries
-- =============

onceCheck = quickCheckWith stdArgs { maxSuccess = 1 }
check1000 = quickCheckWith stdArgs { maxSuccess = 1000, maxDiscard = 1000 }

-- | Comparison allowing for inaccuracy.
cmpE :: (Fractional a, Ord a) => Quantity d a -> Quantity d a -> Quantity d a -> Bool
cmpE accuracy x y' = abs (x - y') < accuracy

cmpP :: (Fractional a, Ord a) => Length a -> CPos a -> CPos a -> Bool
cmpP acc r1 r2 = cmpE acc x1 x2 && cmpE acc y1 y2 && cmpE acc z1 z2
  where
    (x1,y1,z1) = toTuple r1
    (x2,y2,z2) = toTuple r2

-- Accuracies.
dblAcc :: Quantity d Double
dblAcc = Dimensional 1e-8


-- | 3rd party data validation.
prop_va long f x = cmpE e (f cibinong $ perfectGEO $ long*~degree) (x*~degree)
  where
    e = 0.1 *~ degree
    cibinong = GeodeticPlace wgs84
      (negate $ 6*~degree + 26*~arcminute + 52*~arcsecond)
      (106*~degree + 56*~arcminute + 10*~arcsecond)
      (0*~meter)

-- | Converting geocentric to topocentric and back should be identity function.
prop_topo1 place p = not (degeneratePlace place) ==> cmpP dblAcc p p'
  where
    p' = topocentricToGeocentric place $ geocentricToTopocentric place $ p
{-
*** Failed! Falsifiable (after 996 tests):  
GeodeticPlace {refEllips = ReferenceEllipsoid {equatorialRadius = 136781.86798882156 m, polarRadius = 167.72821654743436 m}, latitude = -2243.8289959047747, longitude = 20.39182594235854, height = 13.25536345487733 m}
< -240.34114081130468 m, 102.79932480283043 m, 186.62064762372555 m >
-}


-- | Converting topocentric to geocentric and back should be identity function.
prop_topo2 place p = not (degeneratePlace place) ==> cmpP dblAcc p p'
  where
    p' = geocentricToTopocentric place $ topocentricToGeocentric place $ p


{-
Had a test failure due to degenerate place:

Falsifiable, after 3 tests:
GeodeticPlace {refEllips = ReferenceEllipsoid {equatorialRadius = 4.0 m, polarRadius = 3.0 m}, latitude = 0.0, longitude = -5.0, height = -2.25 m}
< -3.6666666666666665 m, -0.3333333333333333 m, -3.0 m >

Avoided by implementing and using 'degeneratePlace' predicate.
-}
prop_degenerate1 = degeneratePlace place where
  place = GeodeticPlace (ReferenceEllipsoid (4.0*~meter) (3.0*~meter)) _0 _0 ((-2.25)*~meter)

-- | Going to and from az/el/rg observations is id.
prop_obs place p = not (degeneratePlace place) ==> cmpP dblAcc p p'
  where
    az = azimuth   place p
    el = elevation place p
    rg = range     place p
    p' = azElRgToGeocentric place az el rg
{-
*** Failed! Falsifiable (after 867 tests):  
GeodeticPlace {refEllips = ReferenceEllipsoid {equatorialRadius = 136607.51358690596 m, polarRadius = 155.740741518507 m}, latitude = 79.93674030440873, longitude = 60.60648347976257, height = 58.278163555009634 m}
< -16.424578194638578 m, -89.47810248383104 m, -1566.516520378069 m >
*** Failed! Falsifiable (after 295 tests):  
GeodeticPlace {refEllips = ReferenceEllipsoid {equatorialRadius = 276733.3457887228 m, polarRadius = 276730.57152002316 m}, latitude = -152.91564055491563, longitude = -94.14913720744624, height = 187.078596831506 m}
< 95.76534753065962 m, -26.02673518931036 m, -75.65015287140265 m >
-}


-- Arbitrary instances. Should be moved elsewhere...

instance (Arbitrary a) => Arbitrary (Quantity d a) where
  arbitrary   = Dimensional <$> arbitrary

instance Arbitrary a => Arbitrary (CPos a) where
    arbitrary = fromTuple <$> arbitrary

instance (Fractional a, Arbitrary a) => Arbitrary (GeodeticPlace a) where
  {-arbitrary = arbitrary >>= \e    ->
              arbitrary >>= \lat  ->
              arbitrary >>= \long ->
              arbitrary >>= \h    ->
              return (GeodeticPlace e lat long h)-}
  arbitrary = GeodeticPlace <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


instance (Num a, Arbitrary a) => Arbitrary (ReferenceEllipsoid a)
  where
    arbitrary = do
      x <- arbitrary
      let pol = abs x + 1*~meter  -- Prevent zero radius.
      d <- arbitrary
      let eq  = pol + abs d       -- Always larger than polar radius.
      return (ReferenceEllipsoid eq pol)


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
  check1000 prop_topo1
  check1000 prop_topo2
  check1000 prop_obs
