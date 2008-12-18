module Astro.Time.TestUT where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro.Time
import Astro.Time.Interop
import Astro.TimeUT
import Test.QuickCheck
import Data.Time hiding (utc)
import Data.Time.Clock.TAI
import Data.Fixed (Micro, Pico)
import Data.Array.IArray
import qualified Debug.Trace as D

-- Preliminaries
-- =============
onceCheck = check (defaultConfig {configMaxTest = 1})
trace x = D.trace (show x) x

-- Properties
-- ==========
-- readFixed
prop_readInt1 = readFixed " 1   " == (1 :: Pico)
prop_readInt2 = readFixed "-2\t" == (-2 :: Micro)

prop_readPico1 = readFixed "  0.1 " == ( 0.1 :: Pico)
prop_readPico2 = readFixed " -0.1 " == (-0.1 :: Pico)
prop_readPico3 = readFixed "  12.123456789999\t\n" == ( 12.123456789999 :: Pico)
prop_readPico4 = readFixed "\n\t-12.123456789999 " == (-12.123456789999 :: Pico)

prop_readMicro1 = readFixed " 0.1111111" == ( 0.111111 :: Micro)
prop_readMicro2 = readFixed "-0.1111111" == (-0.111112 :: Micro) -- Not really nice...
prop_readMicro3 = readFixed " 0.1111119" == ( 0.111111 :: Micro) -- Not really nice...
prop_readMicro4 = readFixed "-0.1111119" == (-0.111112 :: Micro)

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
  where tai = convertFromUTC (mkLeapSecondTable arr) $ mjd 54831 UTC
prop_UT1_interpolate1 arr = mkUT1Table arr tai ==   0.4066064 *~second - 34*~second
  where tai = convertFromUTC (mkLeapSecondTable arr) $ mjd 54832 UTC
prop_UT1_interpolate_mid arr = mkUT1Table arr tai == (x0 + x1) / _2
  where 
    tai = convertFromUTC (mkLeapSecondTable arr) $ fromUTCTime $ UTCTime (ModifiedJulianDay 54831) 43200.5
    x0 = (-0.5922280)*~second - 33*~second
    x1 =   0.4066064 *~second - 34*~second

-- Conversions
-- -----------
prop_convertUTC t = utc == fromUTCTime (toUTCTime utc) where utc = jd (t::Double) UTC
prop_convertUT1 t = ut1 == fromUniversalTime (toUniversalTime ut1) where ut1 = jd (t::Double) UT1


-- Driver
-- ======
main = do
  arr <- readFile "eop_test.txt" >>= return . mkEOPArray . parseEOPData

  onceCheck prop_readInt1
  onceCheck prop_readInt2
  onceCheck prop_readPico1
  onceCheck prop_readPico2
  onceCheck prop_readPico3
  onceCheck prop_readPico4
  onceCheck prop_readMicro1
  onceCheck prop_readMicro2
  onceCheck prop_readMicro3
  onceCheck prop_readMicro4

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

  quickCheck prop_convertUTC
  quickCheck prop_convertUT1

  -- print $ convertToUT1 (mkUT1Table arr) (clock 2008 10 27 0 0 0 TAI)


