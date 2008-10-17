
{-
[1] http://aa.usno.navy.mil/publications/docs/Circular_179.pdf
[2] IERS TN32
[3] Vallado
-}

module Astro.Time where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Data.Time hiding (utc)
import Data.Time.Clock.TAI
import Data.Fixed (Pico)

-- | We chose to represent our epoch as 'AbsoluteTime's.
type Epoch = AbsoluteTime
diffEpoch :: Fractional a => Epoch -> Epoch -> Time a
diffEpoch e1 e2 = fromDiffTime $ diffAbsoluteTime e1 e2
addTime :: RealFrac a => Epoch -> Time a -> Epoch
addTime e t = addAbsoluteTime (toDiffTime t) e
subTime :: RealFrac a => Epoch -> Time a -> Epoch
subTime e = addTime e . negate

-- | The fractional difference in rate between the time scales TT and TCG. Page viii of [1].
l_G :: Fractional a => Dimensionless a
l_G = 6.969290134e-10 *~ (second / second)

-- | The fractional difference in rate between the time scales TDB and TCB. According to [3] the IAU didn'f fix the exact rate ratio @l_B@ but the value given in [2] is used here.
l_B :: Fractional a => Dimensionless a
l_B = 1.55051976772e-8 *~ (second/second)

-- | The difference between TAI and TT.
taiMinusTT :: Fractional a => Time a
taiMinusTT = (-32.184) *~ second  -- (2.4)


-- | Define a TAI epoch.
tai :: Integer -> Int -> Int -> Int -> Int -> Pico -> Epoch
tai y m d h min s = utcToTAITime (const 0) $
  UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay h min s)

-- | Define a TT epoch. Note that this function isn't monadic!
tt :: Integer -> Int -> Int -> Int -> Int -> Pico -> Epoch
tt y m d h min s = addTime (tai y m d h min s) taiMinusTT

-- | The "standard epoch" J2000.0 (2000-01-01 12:00 TT or JD 2451545.0 TT).
-- Page 9 of [1], page 34 of [2].
j2000 = tt 2000 01 01 12 00 00.000

-- | The epoch at which TT, TCG and ET match. This epoch is 
-- 1977-01-01T00:00:00.000 TAI or 1977-01-01T00:00:32.184 TT|TCG|ET.
convergenceEpoch = tai 1977 01 01 00 00 00.000

-- | Define a TCG epoch. Needs some testing agains reference values...
tcg :: Integer -> Int -> Int -> Int -> Int -> Pico -> Epoch
--tcg y m d h min s = addAbsoluteTime (Prelude.negate $ toDiffTime $ l_G * (fromDiffTime $ diffAbsoluteTime t convergenceEpoch)) t
tcg y m d h min s = t `subTime` (l_G * (diffEpoch t convergenceEpoch))
  where 
    t = tt y m d h min s

-- | Define a TDB (functionally equivalent to Ephemeris Time) epoch. The maximum error is about 
-- 10 microseconds from 1600 to 2200. Formula adapted from page 15 of [1].
-- However, is not within 10 us of TT at 'convergenceEpoch'...
tdb :: Integer -> Int -> Int -> Int -> Int -> Pico -> Epoch
tdb y m d h min s = subTime t0 (tdbMinusTT t0) where t0  = tt y m d h min s

tdbMinusTT :: Floating a => Epoch -> Time a
tdbMinusTT t0 = 0.001657*~second * sin ( 628.3076 *~rpc * t + 6.2401 *~radian)
              + 0.000022*~second * sin ( 575.3385 *~rpc * t + 4.2970 *~radian)
              + 0.000014*~second * sin (1256.6152 *~rpc * t + 6.1969 *~radian)
              + 0.000005*~second * sin ( 606.9777 *~rpc * t + 4.0212 *~radian)
              + 0.000005*~second * sin (  52.9691 *~rpc * t + 0.4444 *~radian)
              + 0.000002*~second * sin (  21.3299 *~rpc * t + 5.5431 *~radian)
              + 0.000010*~(second/century) * t * sin ( 628.3076 *~rpc * t + 4.2490 *~radian)
  where
    rpc = radian / century
    t   = diffEpoch t0 j2000  
      -- ^ This isn't strictly correct, should use final value (iteratively?).
      -- However, the impact is far less than 10 us.
    
tcb :: Integer -> Int -> Int -> Int -> Int -> Pico -> Epoch
tcb y m d h min s = t `subTime` tcbMinusTDB t
  where
    t = tdb y m d h min s

tcbMinusTDB :: Fractional a => Epoch -> Time a
tcbMinusTDB t = l_B * (diffEpoch t convergenceEpoch)


tcb2 y m d h min s = addTime convergenceEpoch $ dTCBdTDB * (diffEpoch t convergenceEpoch)
  where t = tt y m d h min s
dTCBdTDB = _1 - l_B

