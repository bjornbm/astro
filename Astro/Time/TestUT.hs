
import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro.Time
import Astro.TimeUT
import Test.QuickCheck
import Data.Time hiding (utc)
import Data.Time.Clock.TAI
import Data.Array.IArray

-- Preliminaries
-- =============
onceCheck = check (defaultConfig {configMaxTest = 1})


-- Properties
-- ==========
-- Interpolation
prop_interpolate_mid = interpolate (mjd 1 TAI, _1) (mjd 2 TAI, _3) (mjd 1.5 TAI) == _2
prop_interpolate_0   = interpolate (mjd 1 TAI, _1) (mjd 2 TAI, _3) (mjd 1 TAI) == _1
prop_interpolate_1   = interpolate (mjd 1 TAI, _1) (mjd 2 TAI, _3) (mjd 2 TAI) == _3
prop_interpolate_mid' = interpolate (mjd 1 TAI, _1) (mjd 2 TAI, negate _3) (mjd 1.5 TAI) == negate _1

-- UTC
prop_UTC_extrapolate_first arr = mkLeapSecondTable arr (ModifiedJulianDay 0) == 32
prop_UTC_extrapolate_last  arr = mkLeapSecondTable arr (ModifiedJulianDay 100000) == 34
prop_UTC1 arr = mkLeapSecondTable arr (ModifiedJulianDay 53735) == 32
prop_UTC2 arr = mkLeapSecondTable arr (ModifiedJulianDay 53736) == 33

-- UT1
prop_UT1_extrapolate_first arr = mkUT1Table arr (mjd 0 TAI) == (-0.2894287)*~second - 32*~second
prop_UT1_extrapolate_last arr  = mkUT1Table arr (mjd 1e6 TAI) == 0.2098306 *~second - 34*~second
prop_UT1_interpolate0 arr = mkUT1Table arr tai == (-0.5922280)*~second - 33*~second
  where tai = convertFromUTC (mkLeapSecondTable arr) $ UTCTime (ModifiedJulianDay 54831) 0
prop_UT1_interpolate1 arr = mkUT1Table arr tai ==   0.4066064 *~second - 34*~second
  where tai = convertFromUTC (mkLeapSecondTable arr) $ UTCTime (ModifiedJulianDay 54832) 0
prop_UT1_interpolate_mid arr = mkUT1Table arr tai == (x0 + x1) / _2
  where 
    tai = convertFromUTC (mkLeapSecondTable arr) $ UTCTime (ModifiedJulianDay 54831) 43200.5
    x0 = (-0.5922280)*~second - 33*~second
    x1 =   0.4066064 *~second - 34*~second


-- Driver
-- ======
main = do
  arr <- readFile "eop_test.txt" >>= return . mkEOPArray . parseEOPData

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

  print $ convertToUT1 (mkUT1Table arr) (clock 2008 10 27 0 0 0 TAI)

