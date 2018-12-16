module Astro.TestCelestrak where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro.Time
import Astro.Time.Interop
import Astro.Celestrak
import Test.QuickCheck
import Data.Time hiding (utc)
import Data.Time.Clock.TAI
import Data.Array.IArray
import qualified Debug.Trace as D

-- Preliminaries
-- =============
onceCheck = quickCheckWith stdArgs { maxSuccess = 1 }
trace x = D.trace (show x) x

-- | Comparison allowing for inaccuracy.
cmpE :: (Fractional a, Ord a) => Time a -> E t a -> E t a -> Bool
cmpE accuracy t t' = abs (diffEpoch t t') < accuracy

-- Properties
-- ==========
-- Interpolation
prop_interpolate_mid = interpolate (mjd 1 TAI, _1) (mjd 2 TAI, _3) (mjd 1.5 TAI) == _2
prop_interpolate_0   = interpolate (mjd 1 TAI, _1) (mjd 2 TAI, _3) (mjd 1 TAI) == _1
prop_interpolate_1   = interpolate (mjd 1 TAI, _1) (mjd 2 TAI, _3) (mjd 2 TAI) == _3
prop_interpolate_mid' = interpolate (mjd 1 TAI, _1) (mjd 2 TAI, negate _3) (mjd 1.5 TAI) == negate _1

-- UTC
prop_UTC_extrapolate_first arr = mkLeapSecondMap arr (ModifiedJulianDay 0) == Just 32
prop_UTC_extrapolate_last  arr = mkLeapSecondMap arr (ModifiedJulianDay 100000) == Just 34
prop_UTC1 arr = mkLeapSecondMap arr (ModifiedJulianDay 53735) == Just 32
prop_UTC2 arr = mkLeapSecondMap arr (ModifiedJulianDay 53736) == Just 33

-- UT1
prop_UT1_extrapolate_first arr = mkUT1Table arr (mjd 0 TAI) == (-0.2894287)*~second - 32*~second
prop_UT1_extrapolate_last arr  = mkUT1Table arr (mjd 1e6 TAI) == 0.2098306 *~second - 34*~second
prop_UT1_interpolate0 arr = mkUT1Table arr tai == (-0.5922280)*~second - 33*~second
  where tai = utcToTAI (mkLeapSecondMap arr) $ clockUTC 2008 12 31 0 0 0
prop_UT1_interpolate1 arr = mkUT1Table arr tai ==   0.4066064 *~second - 34*~second
  where tai = utcToTAI (mkLeapSecondMap arr) $ clockUTC 2009 01 01 0 0 0
prop_UT1_interpolate_mid arr = mkUT1Table arr tai == (x0 + x1) / _2
  where
    tai = utcToTAI (mkLeapSecondMap arr) $ UTCTime (ModifiedJulianDay 54831) 43200.5
    x0 = (-0.5922280)*~second - 33*~second
    x1 =   0.4066064 *~second - 34*~second

-- Conversions
-- -----------
-- prop_convertUTC t = utc == fromUTCTime (toUTCTime utc) where utc = jd (t::Double) UTC
prop_convertUT1 t = cmpE (1e-4*~second) ut1 $ fromUniversalTime (toUniversalTime ut1) where ut1 = jd (t::Double) UT1


-- Driver
-- ======
main = do
  arr <- readFile "test/testdata/eop_test.txt" >>= return . mkEOPArray . parseEOPData

  onceCheck prop_interpolate_mid
  onceCheck prop_interpolate_0
  onceCheck prop_interpolate_1
  onceCheck prop_interpolate_mid'

  onceCheck $ prop_UTC_extrapolate_first arr
  onceCheck $ prop_UTC_extrapolate_last  arr
  onceCheck $ prop_UTC1 arr
  onceCheck $ prop_UTC2 arr

  onceCheck $ prop_UT1_extrapolate_first arr
  onceCheck $ prop_UTC_extrapolate_last  arr
  onceCheck $ prop_UT1_interpolate0 arr
  onceCheck $ prop_UT1_interpolate1 arr
  onceCheck $ prop_UT1_interpolate_mid arr

  --quickCheck prop_convertUTC
  quickCheck prop_convertUT1

  -- print $ convertToUT1 (mkUT1Table arr) (clock 2008 10 27 0 0 0 TAI)
