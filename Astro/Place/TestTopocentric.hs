module Astro.Place.TestTopocentric where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro.Place
import Astro.Place.Topocentric
import Astro.Place.ReferenceEllipsoid
import Astro.Util (perfectGEO)
import Test.QuickCheck


-- Preliminaries
-- =============

onceCheck = check (defaultConfig {configMaxTest = 1})

-- | Comparison allowing for inaccuracy.
cmpE :: (Fractional a, Ord a) => Quantity d a -> Quantity d a -> Quantity d a -> Bool
cmpE accuracy x y' = abs (x - y') < accuracy

-- Accuracies.


prop_va long f x = cmpE e (f cibinong $ perfectGEO $ long*~degree) (x*~degree)
  where
    e = 0.1 *~ degree
    cibinong = GeodeticPlace wgs84
      (negate $ 6*~degree + 26*~arcminute + 52*~arcsecond)
      (106*~degree + 56*~arcminute + 10*~arcsecond)
      (0*~meter)


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
