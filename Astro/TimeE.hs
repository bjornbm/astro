{-# LANGUAGE ScopedTypeVariables
           , MultiParamTypeClasses
           , FlexibleInstances
  #-}



module Astro.TimeE (
  -- * References
  -- $ref

  -- * Time Scales
  E
  -- ** International Atomic Time (TAI)
  , TAI (TAI)
  , wrapTAI
  , unwrapTAI
  -- ** Terrestial Time (TT)
  -- $tt
  , TT  (TT)
  , j2000
  -- ** Geocentric Coordinate Time (TCG)
  -- $tcg
  , TCG (TCG)
  -- ** Barycentric Dynamical Time (TDB)
  -- $tdb
  , TDB (TDB)
  -- ** Barycentric Coordinate Time (TCB)
  -- $tcb
  , TCB (TCB)
  , convert
  -- * Time Respresentations
  -- ** Clock Time
  , clock
  , showClock
  -- ** Julian Dates (JD)
  -- $jd
  , jd
  , showJD
  -- ** Modified Julian Dates (MJD)
  -- $mjd
  , mjd
  , showMJD
  -- * Subject To Change
  , DiffEpoch
  , diffEpoch
  ) where


import Numeric.Units.Dimensional.Prelude hiding (century)
import qualified Prelude
import Data.Time hiding (utc)
import qualified Data.Time (utc)
import Data.Time.Clock.TAI
import Data.Fixed (Pico)


-- A Julian century.
century :: Num a => Unit DTime a
century = prefix 36525 day


-- | Representation of an epoch parameterized by time scale.
newtype E t = E AbsoluteTime deriving (Eq, Ord)
instance Show t => Show (E t) where show = showClock


class DiffEpoch t where 
  -- | Obtain the difference in seconds between two epochs. Beware that if
  -- the epochs' time scale isn't based on SI seconds the result won't be
  -- in SI seconds.
  diffEpoch :: Fractional a => t -> t -> Time a
  -- | Add time to an epoch.
  addTime   :: RealFrac   a => t -> Time a -> t
  -- | Subtract time from an epoch.
  subTime   :: RealFrac   a => t -> Time a -> t
  subTime e = addTime e . negate
instance DiffEpoch AbsoluteTime where
  diffEpoch t t' = fromDiffTime $ diffAbsoluteTime t t'
  addTime   t dt = addAbsoluteTime (toDiffTime dt) t
instance DiffEpoch (E t) where 
  diffEpoch (E t) (E t') = diffEpoch t t'
  addTime   (E t)    dt  = E $ addTime t dt


-- Convenience operators. These allow our function definitions to more
-- closely resemble the formulae they implement.
infixl 6 .+, .-
(.+) :: (DiffEpoch t, RealFrac a) => t -> Time a -> t
(.+) = addTime
(.-) :: (DiffEpoch t, Fractional a) => t -> t ->  Time a
(.-) = diffEpoch



-- * Time Representations
-- ** Clock dates
--    -----------
-- | Define an epoch using "clock time" and time scale. 
clock :: Integer -> Int -> Int -> Int -> Int -> Pico -> t -> E t
clock y m d h min s _ = E $ utcToTAITime (const 0) $
  UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay h min s)

-- | A helper function. Identical to 'clock' but the time scale comes first.
clock' :: t -> Integer -> Int -> Int -> Int -> Int -> Pico -> E t
clock' t y m d h min s = clock y m d h min s t

-- | Show an epoch as a clock time. This function is used by the @Show@
-- instance. TODO: should change this to use ISO8601 format.
showClock :: forall t. Show t => E t -> String
showClock (E t) = show (utcToLocalTime Data.Time.utc (taiToUTCTime (const 0) t)) 
               ++ ' ':show (undefined::t)


-- ** Julian Date (JD)
--    ----------------
{- $jd
A Julian Date (JD) is a continuous count of days and fractions elapsed
since an initial epoch. The integral part is the Julian day number.
The fractional part is the time of day since noon UT as a decimal fraction
of one day, with 0.5 representing midnight. [WPJD]

The initial epoch is defined as noon, January 1, 4713 BC in the
proleptic Julian calendar. Traditionally JD has been used with the UT1
time scale but it is common (see e.g. [C179]) to specify Julian dates
in other time scales.
-}

-- | The epoch of JD 0.0 as an AbsoluteTime.
jdEpoch :: AbsoluteTime
jdEpoch  = subTime mjdEpoch (2400000.5 *~ day)

-- | Define an epoch by specifying a JD and time scale.
jd :: RealFrac a => a -> t -> E t
jd d _ = E (addTime jdEpoch (d *~ day))

-- | Show an epoch as JD on the format \"JD 0.0 TAI\".
showJD :: forall t. Show t => E t -> String
showJD (E t) = "JD " ++ show (diffEpoch t jdEpoch /~ day) ++ " " ++ show (undefined::t)


-- ** Modified Julian Date (MJD)
--    --------------------------
{- $mjd
A Modified Julian Date (MJD) is analogous with a Julian Date but with
an initial epoch of 1858-11-17 00:00.
-}

-- | The epoch of MJD 0.0 as an AbsoluteTime.
mjdEpoch :: AbsoluteTime
mjdEpoch = taiEpoch

-- | Define an epoch by specifying a MJD and time scale.
mjd :: RealFrac a => a -> t -> E t
mjd d _ = E (addTime mjdEpoch (d *~ day))

-- | Show an epoch as MJD on the format \"MJD 0.0 TAI\".
showMJD :: forall t. Show t => E t -> String
showMJD (E t) = "MJD " ++ show (diffEpoch t mjdEpoch /~ day) ++ " " ++ show (undefined::t)



-- * Time scales
-- ===========
{-
For the SI-based time scales, the event tagged 1977 January 1,
00:00:00 TAI (JD 2443144.5 TAI) at the geocenter is special. At
that event, the time scales TT, TCG, and TCB all read 1977 January
1, 00:00:32.184 (JD 2443144.5003725). (The 32.184 s offset is the
estimated difference between TAI and the old Ephemeris Time scale.)
This event will be designated the //convergence epoch// in the
following; it can be represented in any of the time scales, and the
context will dictate which time scale is appropriate. [C179]
-}

-- ** International Atomic Time (TAI)
--    -------------------------------
data TAI = TAI; instance Show TAI where show _ = "TAI"

-- | Converts a 'Data.Time.Clock.TAI.AbsoluteTime' into this module's
-- representation of an epoch. (Perhaps the use of \"wrap\" in the function
-- name is inappropriate as it leaks/implies implementation details.)
wrapTAI :: AbsoluteTime -> E TAI
wrapTAI = E

-- | Converts this module's representation of a TAI epoch into a
-- 'Data.Time.Clock.TAI.AbsoluteTime'.
unwrapTAI :: E TAI -> AbsoluteTime
unwrapTAI (E t) = t

-- | The epoch at which TT, TCG and TDB all read 1977-01-01T00:00:32.184.
convergenceEpochTAI :: E TAI
convergenceEpochTAI = clock 1977 01 01 00 00 00.000 TAI
convergenceEpochTT  = clock 1977 01 01 00 00 32.184 TT
convergenceEpochTCG = clock 1977 01 01 00 00 32.184 TCG
convergenceEpochTCB = clock 1977 01 01 00 00 32.184 TCB


-- ** Terrestial Time (TT)
--    --------------------
{- $tt
The astronomical time scale called Terrestrial Time (TT) is used
widely for geocentric and topocentric ephemerides and runs at the
same rate as a time scale based on SI seconds on the surface of the
Earth. TT can be considered an idealized form of TAI with an epoch
offset: TT = TAI + 32.184 s. This expressssion for TT preserves
continuity with previously-used (now obsolete) \"dynamical\" time
scales, Terrestrial Dynamical Time (TDT) and Ephemeris Time (ET).
That is, ET -> TDT -> TT can be considered a single continuous time
scale.
-}
data TT  = TT ; instance Show TT  where show _ = "TT"

-- | The "standard epoch" J2000.0 (2000-01-01 12:00 TT or JD 2451545.0 TT).
-- Page 9 of [1], page 34 of [2].
j2000 = clock 2000 01 01 12 00 00.000 TT

-- | The difference between TAI and TT.
ttMinusTAI :: Time Pico
ttMinusTAI = 32.184 *~ second  -- (2.4)

-- | Convert a TAI epoch into a TT epoch.
taiToTT :: E TAI -> E TT
taiToTT (E t) = E $ addTime t ttMinusTAI

-- | Convert a TT epoch into a TAI epoch.
ttToTAI :: E TT -> E TAI
ttToTAI (E t) = E $ subTime t ttMinusTAI



-- * Theoretical Time Scales
-- =======================
{-
dafda
fafa
afa
-}

-- ** Geocentric Coordinate Time (TCG)
--   --------------------------------
{- $tcg
The coordinate time of the Geocentric Celestial Reference System
(GCRS), which advances by SI seconds within that system. [AAG]
-}
data TCG = TCG; instance Show TCG where show _ = "TCG"

-- | The fractional difference in rate between the time scales TT and TCG.
-- Page viii of [1].
l_G :: Fractional a => Dimensionless a
l_G = 6.969290134e-10 *~ (second / second)

-- | The difference between the TT and TCG time scales as a function of
-- TCG epoch. The formula used is exact, barring numerical errors.
ttMinusTCG :: Fractional a => E TCG -> Time a
ttMinusTCG tcg = negate l_G * (tcg .- convergenceEpochTCG)

{-
The exact mathematical expression for TCG with TT as the free variable is

  TCG = (TT - t0) / (1 - L_G) + t0

where t0 is the convergence epoch (identical for TT and TCG). However,
this expression doesn't lend it self well to implementations based on
double precision arithmetic. In particular the term (1 - L_G) suffers a
significant loss of precision. Also, the error in (TT - t0) is carried
over undiminished to TCG.

We follow the suggestion on page 15 of [dav] and expand the fraction
to obtain

  TCG = (TT - t0) (1 + L_G + L_G^2 + L_G^3 ...) + t0
      = TT + (TT - t0) (L_G + L_G^2 + L_G^3 ...)

This expression avoids both precision problems mentioned
above. Furthermore, for double precision terms beyond L_G^2 are
insignificant.
-}

-- | The difference between the TCG and TT time scales as a function of
-- TT epoch. This function implements Eq (27) of [dav].
tcgMinusTT :: Fractional a => E TT -> Time a
tcgMinusTT tt = (tt .- convergenceEpochTT) * (l_G + l_G^pos2)  -- (27) of [dav].

-- | Convert a TCG epoch into a TT epoch.
tcgToTT :: E TCG -> E TT
tcgToTT tcg@(E t) = E $ addTime t (ttMinusTCG tcg)

-- | Convert a TT epoch into a TCG epoch.
ttToTCG :: E TT -> E TCG
ttToTCG tt@(E t) = E $ addTime t (tcgMinusTT tt)



-- ** Barycentric Dynamical Time (TDB)
--    --------------------------------
{- $tdb
A time scale defined by the IAU (originally in 1976; named in 1979;
revised in 2006) for use as an independent argument of barycentric
ephemerides and equations of motion. TDB is a linear function of
Barycentric Coordinate Time (TCB) that on average tracks TT over
long periods of time; differences between TDB and TT evaluated at
the Earth's surface remain under 2 ms for several thousand years
around the current epoch. TDB is functionally equivalent to T_eph,
the independent argument of the JPL planetary and lunar ephemerides
DE405/LE405. (From [AAG].)

In terms of implementing TDB the defintion as a linear relation
with TCB is of limited utility in computing TDB from other time
scales as TCB lacks a concrete realization. Instead we will use an
imprecise expression relating TDB to TT. (This expression, together
with the definition of TDB, is also used to obtain TCB.)
-}
data TDB = TDB; instance Show TDB where show _ = "TDB"

-- | The difference between the TDB and TT time scales as a function of
-- TT epoch. This formula is adapted from (2.6) of [C179] and reportedly
-- has a maximum error of about 10 microseconds between the years 1600 
-- and 2200.
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

{-
We incorrectly substitute TDB for TT in 'ttMinusTDB' but the error
introduced by this approximation is less than a picosecond (far less than
the 10 microsecond accuracy inherent in the formula in the first place).
-}

-- | The difference between the TDB and TT time scales as a function of
-- TT epoch. The maximum error is about 10 microseconds from 1600 to 2200.
-- Adapted from (2.6) of [C179].
ttMinusTDB :: Floating a => E TDB -> Time a
ttMinusTDB (E t) = negate $ tdbMinusTT (E t)

-- | Convert a TT epoch into a TDB epoch.
ttToTDB :: E TT -> E TDB
ttToTDB tt@(E t) = E $ addTime t (tdbMinusTT tt)

-- | Convert a TDB epoch into a TT epoch.
tdbToTT :: E TDB -> E TT
tdbToTT tdb@(E t) = E $ addTime t (ttMinusTDB tdb)



-- ** Barycentric Coordinate Time (TCB)
--    ---------------------------------
{- $tcb
The coordinate time of the Barycentric Celestial Reference System
(BCRS), which advances by SI seconds within that system. [AAG]
-}
data TCB = TCB; instance Show TCB where show _ = "TCB"

-- | The fractional difference in rate between the time scales TDB and TCB.
-- @l_B@ was given an exact value and declared a defining constant in [B3].
l_B :: Fractional a => Dimensionless a
l_B = 1.550519768e-8 *~ (second/second)

{- |
The difference between TCB and TDB time scales at the convergence
epoch 1977 January 1.0 TAI. This difference is a defining constant
from [B3] as opposed to the epoch calculated using the TDB conversion
formulae provided in this module.  As a result, since calculation
of TCB relies on an intermediate calculation of TDB a calculation
of TCB at the convergence epoch will not produce 1977-01-01
00:00:32.184 TCB exactly. The difference is introduced by the
intermediate TDB calculation and is within its error margin.
-}
tdb_0 :: Fractional a => Time a
tdb_0 = (-6.55e-5) *~ second  -- :: Time Pico

-- | The TDB epoch corresponding to 1977 January 1.0 TAI.
convergenceEpochTDB :: E TDB
convergenceEpochTDB = E $ t .+ (tdb_0 :: Time Pico)
  where
    E t = convergenceEpochTCG

-- | The difference between the TDB and TCB time scales as a function of
-- TCB epoch.
tdbMinusTCB :: Fractional a => E TCB -> Time a
tdbMinusTCB tcb = tdb_0 - (tcb .- convergenceEpochTCB) * l_B

{-
'tcbMinusTDB' is defined analogously with 'tcgMinusTT' (see commentary
above). Note that since L_B is two orders of magnitude larger than L_G
the L_B^3 term does have a very small effect.
-}

-- | The difference between the TCB and TDB time scales as a function of
-- TDB epoch.
tcbMinusTDB :: Fractional a => E TDB -> Time a
tcbMinusTDB tdb = (tdb .- convergenceEpochTDB) * (l_B + l_B^pos2 + l_B^pos3) - tdb_0

-- | Convert a TCB epoch into a TDB epoch.
tcbToTDB :: E TCB -> E TDB
tcbToTDB tcb@(E t) = E $ addTime t (tdbMinusTCB tcb)

-- | Convert a TDB epoch into a TCB epoch.
tdbToTCB :: E TDB -> E TCB
tdbToTCB tdb@(E t) = E $ addTime t (tcbMinusTDB tdb)



-- Conversion
-- ==========

class Convert t t' where
  -- | Convert between time scales. The target of the conversion is
  -- decided from the expected type (provide a type signature if you
  -- need to be explicit).
  convert :: E t -> E t'

-- Since the number or time scales is limited we "brute force" the instance
-- definitions instead of trying to get fancy.

instance Convert a a where convert = id  -- Trivial.

instance Convert TAI TT  where convert = taiToTT
instance Convert TAI TCG where convert = convert . taiToTT
instance Convert TAI TDB where convert = convert . taiToTT
instance Convert TAI TCB where convert = convert . taiToTT

instance Convert TT  TAI where convert =  ttToTAI
instance Convert TT  TCG where convert =  ttToTCG
instance Convert TT  TDB where convert =  ttToTDB
instance Convert TT  TCB where convert =  convert . ttToTDB

instance Convert TCG TAI where convert = convert . tcgToTT
instance Convert TCG TT  where convert = tcgToTT
instance Convert TCG TDB where convert = convert . tcgToTT
instance Convert TCG TCB where convert = convert . tcgToTT

instance Convert TDB TAI where convert = convert . tdbToTT
instance Convert TDB TT  where convert = tdbToTT
instance Convert TDB TCG where convert = convert . tdbToTT
instance Convert TDB TCB where convert = tdbToTCB

instance Convert TCB TAI where convert = convert . tcbToTDB
instance Convert TCB TT  where convert = convert . tcbToTDB
instance Convert TCB TCG where convert = convert . tcbToTDB
instance Convert TCB TDB where convert = tcbToTDB


-- References
-- ==========
{-$ref

 * [C179] <http://aa.usno.navy.mil/publications/docs/Circular_179.php>

 * [B3]   <http://www.iau.org/static/resolutions/IAU2006_Resol3.pdf>

 * [dav]  <http://www.centerforspace.com/downloads/files/pubs/AAS-06-134.pdf>

 * [AAG]  <http://asa.usno.navy.mil/SecM/Glossary.html>
 
 * [WPJD] <http://en.wikipedia.org/wiki/Julian_date>

-}

