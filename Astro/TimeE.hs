module Astro.TimeE where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Data.Time hiding (utc)
import qualified Data.Time (utc)
import Data.Time.Clock.TAI
import Data.Fixed (Pico)


-- | We chose to represent our epoch as 'AbsoluteTime's.
type Epoch = AbsoluteTime

class DiffEpoch t where 
  -- | Obtain the difference in seconds between two epochs. Beware that if
  -- the time scale of the epoch isn't based on SI seconds the result won't
  -- be SI seconds!
  diffEpoch :: Fractional a => t -> t -> Time a
instance DiffEpoch AbsoluteTime where
  diffEpoch t t' = fromDiffTime $ diffAbsoluteTime t t'
instance DiffEpoch (E t) where diffEpoch (E t) (E t') = diffEpoch t t'

addTime :: RealFrac a => Epoch -> Time a -> Epoch
addTime e t = addAbsoluteTime (toDiffTime t) e
subTime :: RealFrac a => Epoch -> Time a -> Epoch
subTime e = addTime e . negate


-- | A wrapper for tagging an AbsoluteTime with a time scale.
newtype E t = E AbsoluteTime deriving (Eq, Ord)
instance Show t => Show (E t) where show = showClock

-- | Epochs in a time scale must be convertable to TAI for it to be useful(?).
class T t where
  toTAI :: E t -> AbsoluteTime
  fromTAI :: AbsoluteTime -> E t


-- Clock dates
-- ===========
-- | Define an epoch using "clock time" and time scale. 
clock :: Integer -> Int -> Int -> Int -> Int -> Pico -> t -> E t
clock y m d h min s _ = E $ utcToTAITime (const 0) $
  UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay h min s)

-- | A helper function. Identical to 'clock' but the time scale comes first.
clock' :: t -> Integer -> Int -> Int -> Int -> Int -> Pico -> E t
clock' t y m d h min s = clock y m d h min s t

-- | Show an epoch in "clock".
showClock :: forall t. Show t => E t -> String
showClock (E t) = show (utcToLocalTime Data.Time.utc (taiToUTCTime (const 0) t)) 
               ++ ' ':show (undefined::t)


-- Julian dates
-- ============

-- Modified Julian Date (MJD)
-- --------------------------
-- | The epoch of MJD 0.0.
mjdEpoch = taiEpoch

-- | Define an epoch by specifying a MJD and time scale.
mjd :: RealFrac a => a -> t -> E t
mjd d _ = E (addTime mjdEpoch (d *~ day))

-- | Show an epoch as MJD.
showMJD :: forall t. Show t => E t -> String
showMJD (E t) = "MJD " ++ show (diffEpoch t mjdEpoch /~ day) ++ " " ++ show (undefined::t)


-- Julian Date (JD)
-- ----------------
-- | The epoch of JD 0.0.
jdEpoch  = subTime mjdEpoch (2400000.5 *~ day)

-- | Define an epoch by specifying a JD and time scale.
jd :: RealFrac a => a -> t -> E t
jd d _ = E (addTime jdEpoch (d *~ day))

-- Show an epoch as JD.
showJD :: forall t. Show t => E t -> String
showJD (E t) = "JD " ++ show (diffEpoch t jdEpoch /~ day) ++ " " ++ show (undefined::t)


-- Time scales
-- ===========

-- International Atomic Time (TAI)
-- -------------------------------
data TAI = TAI; instance Show TAI where show _ = "TAI"

instance T TAI where 
  toTAI (E t) = t
  fromTAI t = E t

j2000tai = clock 2000 01 01 12 00 00.000 TAI

-- | The epoch at which TT, TCG and TDB all read 1977-01-01T00:00:32.184.
convergenceEpochTAI = clock 1977 01 01 00 00 00.000 TAI
convergenceEpochTT  = clock 1977 01 01 00 00 32.184 TT
convergenceEpochTCG = clock 1977 01 01 00 00 32.184 TCG


-- Terrestial Time (TT)
-- --------------------
data TT  = TT ; instance Show TT  where show _ = "TT"

-- | The difference between TAI and TT.
ttMinusTAI :: Time Pico
ttMinusTAI = 32.184 *~ second  -- (2.4)

instance T TT where
  toTAI (E t) = subTime t ttMinusTAI
  fromTAI t = E (addTime t ttMinusTAI)

-- | The "standard epoch" J2000.0 (2000-01-01 12:00 TT or JD 2451545.0 TT).
-- Page 9 of [1], page 34 of [2].
j2000 = clock 2000 01 01 12 00 00.000 TT


-- Geocentric Coordinate Time (TCG)
-- --------------------------------
data TCG = TCG; instance Show TCG where show _ = "TCG"

-- | The fractional difference in rate between the time scales TT and TCG. Page viii of [1].
l_G :: Fractional a => Dimensionless a
l_G = 6.969290134e-10 *~ (second / second)

-- | The difference between the TT and TCG time scales as a function of
-- TCG epoch. The formula used is exact.
ttMinusTCG :: Fractional a => E TCG -> Time a
ttMinusTCG tcg = negate l_G * (tcg `diffEpoch` convergenceEpochTCG)

-- | The difference between the TCG and TT time scales as a function of
-- TT epoch. The inaccuracy of the formula used should be "negligibly small while
-- using double-precision (8-byte) words" (page 15 of [Vallado]).
tcgMinusTT :: Fractional a => E TT -> Time a
tcgMinusTT tt = (tt `diffEpoch` convergenceEpochTT) * (l_G + l_G^pos2 + l_G^pos3)  -- (27) of [Vallado].

-- | Convert a TCG epoch into a TT epoch.
tcgToTT :: E TCG -> E TT
tcgToTT tcg@(E t) = E $ addTime t (ttMinusTCG tcg)

-- | Convert a TT epoch into a TCG epoch.
ttToTCG :: E TT -> E TCG
ttToTCG tt@(E t) = E $ addTime t (tcgMinusTT tt)

instance T TCG where
  toTAI = toTAI . tcgToTT
  fromTAI = ttToTCG . fromTAI


-- Barycentric Dynamical Time (TDB)
-- --------------------------------
data TDB = TDB; instance Show TDB where show _ = "TDB"

-- | The difference between the TDB and TT time scales as a function of
-- TT epoch. The maximum error is about 10 microseconds from 1600 to 2200.
-- Adapted from (2.6) of [Kaplan].
tdbMinusTT :: Floating a => E TT -> Time a
tdbMinusTT tt = 0.001657*~second * sin ( 628.3076 *~rpc * t + 6.2401 *~radian)
              + 0.000022*~second * sin ( 575.3385 *~rpc * t + 4.2970 *~radian)
              + 0.000014*~second * sin (1256.6152 *~rpc * t + 6.1969 *~radian)
              + 0.000005*~second * sin ( 606.9777 *~rpc * t + 4.0212 *~radian)
              + 0.000005*~second * sin (  52.9691 *~rpc * t + 0.4444 *~radian)
              + 0.000002*~second * sin (  21.3299 *~rpc * t + 5.5431 *~radian)
              + 0.000010*~(second/century) * t * sin ( 628.3076 *~rpc * t + 4.2490 *~radian)
  where
    rpc = radian / century
    t   = diffEpoch tt j2000  
      -- ^ This isn't strictly correct, should use final value (iteratively?).
      -- However, the impact is far less than 10 us.

-- | The difference between the TDB and TT time scales as a function of
-- TT epoch. The maximum error is about 10 microseconds from 1600 to 2200.
-- Adapted from (2.6) of [Kaplan]. We incorrectly substitute TDB for TT in
-- the formula but the error introduced by this approximation is far less
-- than the 10 microsecond accuracy inherent in (2.6).
ttMinusTDB :: Floating a => E TDB -> Time a
ttMinusTDB (E t) = negate $ tdbMinusTT (E t)

-- | Convert a TT epoch into a TDB epoch.
ttToTDB :: E TT -> E TDB
ttToTDB tt@(E t) = E $ addTime t (tdbMinusTT tt)

-- | Convert a TDB epoch into a TT epoch.
tdbToTT :: E TDB -> E TT
tdbToTT tdb@(E t) = E $ addTime t (ttMinusTDB tdb)

instance T TDB where
  toTAI = toTAI . tdbToTT
  fromTAI = ttToTDB . fromTAI


-- Barycentric Coordinate Time
-- ---------------------------
data TCB = TCB; instance Show TCB where show _ = "TCB"

-- | The fractional difference in rate between the time scales TDB and TCB.
-- According to [3] the IAU didn'f fix the exact rate ratio @l_B@ but the
-- value given in [2] is used here.
l_B :: Fractional a => Dimensionless a
l_B = 1.55051976772e-8 *~ (second/second)

-- | The difference between the TCB and TDB time scales as a function of
-- TDB epoch. The TDB epoch is converted to the independent TAI epoch in
-- (30) of [Vallado]. TODO: Validate with [McCarthy], would have expected
-- analogy with TCG....
-- This formula means that TDB and TCG are identical at the convergence
-- epoch. I don't believe that make sense because TDB /= TT at the conv.
-- epoch...
tcbMinusTDB :: Floating a => E TDB -> Time a
tcbMinusTDB tdb = l_B * (diffEpoch (fromTAI $ toTAI tdb) convergenceEpochTAI) + (6.7243915e-5 *~ second)
                                                                              -- ^ Adaptation to meet convergence epoch...
tcbMinusTDB' :: Floating a => E TAI -> Time a
tcbMinusTDB' tai = l_B * (diffEpoch tai convergenceEpochTAI)

-- | The difference between the TDB and TCB time scales as a function of
-- TCB epoch. The TDB epoch is converted to the independent TAI epoch in
tdbMinusTCB :: Floating a => E TCB -> Time a
tdbMinusTCB tcg = undefined -- TODO

-- | Convert a TDB epoch into a TCB epoch.
tdbToTCB :: E TDB -> E TCB
tdbToTCB tdb@(E t) = E $ addTime t (tcbMinusTDB tdb)

-- | Convert a TCB epoch into a TDB epoch.
tcbToTDB :: E TCB -> E TDB
tcbToTDB tcb@(E t) = E $ addTime t (tdbMinusTCB tcb)

instance T TCB where
  fromTAI = tdbToTCB . fromTAI
  toTAI = toTAI . tcbToTDB

