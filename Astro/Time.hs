{-# LANGUAGE ScopedTypeVariables
           , MultiParamTypeClasses
           , FlexibleInstances
  #-}

{- |
   Copyright  : Copyright (C) 2008 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : experimental
   Portability: GHC only?

Time scales and time conversion facilities relevant to astrodynamics.

The barycentric time scales are Barycentric Coordinate Time (TCB)
and Barycentric Dynamical Time (TDB) where the latter is defined
as a linear function of former. A concrete implementation of
conversions between barycentric and terrestrial time scales isn't
provided in this module. Instead a module in the @Astro.Time.Barycentric@
namespace should be imported according to accuracy/performance
requirements.

(In terms of implementing TDB the defintion as a linear relation
with TCB is of limited utility in computing TDB from other time
scales as TCB lacks a concrete realization. Instead one would
generally use an (imprecise) expression relating TDB to TT. This
expression, together with the definition of TDB, would then also
be used to obtain TCB.)

For interoperation with the types of the time library ("Data.Time"
module hierarchy) see the "Astro.Time.Interop" module.

-}
module Astro.Time (
  -- * References
  -- $ref

  -- * Time Scales
  E (E)
  , coerce
  --, Convert
  --, convert
  -- * Terrestrial Time Scales
  -- ** International Atomic Time (TAI)
  -- $tai
  , TAI (TAI)
  -- ** Terrestial Time (TT)
  -- $tt
  , TT  (TT)
  , j2000
  , sinceJ2000
  -- ** Geocentric Coordinate Time (TCG)
  -- $tcg
  , TCG (TCG)
  -- ** Universal Time
  , UT1 (UT1)
  -- * Barycentric Time Scales
  -- ** Barycentric Dynamical Time (TDB)
  -- $tdb
  , TDB (TDB)
  -- ** Barycentric Coordinate Time (TCB)
  -- $tcb
  , TCB (TCB)
  -- * Time Respresentations
  -- ** Clock Time
  , clock
  , clock'
  , showClock
  -- ** Julian Dates (JD)
  -- $jd
  , jd
  , jd'
  , showJD
  -- ** Modified Julian Dates (MJD)
  -- $mjd
  , mjd
  , mjd'
  , showMJD
  -- * Subject To Change
  , diffEpoch
  , addTime
  , century
  --  * Conversion
  , taiToTT ,  ttToTAI
  , tcgToTT ,  ttToTCG
  , tcbToTDB, tdbToTCB
  , taiToUT1, ut1ToTAI, UT1MinusTAI, TAIMinusUT1
  -- $unsafe
  , unsafeToModJulianDate, unsafeFromModJulianDate
  ) where


import Numeric.Units.Dimensional.Prelude  -- hiding (century)
import qualified Prelude
import Data.Time hiding (utc)
import qualified Data.Time (utc)
import Data.Time.Clock.TAI
import Data.Fixed (Pico)


-- A Julian century.
century :: Num a => Unit DTime a
century = prefix 36525 day


-- | Representation of an epoch parameterized by time scale and
-- numerical representation. When Double is used for the numerical
-- representation the accuracy will be below one microsecond in
-- the 21st century. For exact numerical representation use e.g.
-- Rational.
newtype E t a = E (Time a) deriving (Eq, Ord)
instance (Show t, Show a, Real a, Fractional a)
  => Show (E t a) where show = showClock


-- | Obtain the difference in seconds between two epochs. Beware that if
-- the epochs' time scale isn't based on SI seconds the result won't be
-- in SI seconds.
diffEpoch :: Num a => E t a -> E t a -> Time a
diffEpoch (E t) (E t') = t - t'
-- | Add time to an epoch.
addTime :: Num a => E t a -> Time a -> E t a
addTime (E t) dt  = E $ t + dt
-- | Subtract time from an epoch.
subTime :: Num a => E t a -> Time a -> E t a
subTime e = addTime e . negate


-- Convenience operators. These allow our function definitions to more
-- closely resemble the formulae they implement.
infixl 6 .+, .-
(.+) :: Num a => E t a -> Time a -> E t a
(.+) = addTime
(.-) :: Num a => E t a -> E t a ->  Time a
(.-) = diffEpoch


{- $unsafe

These functions convert epochs to and from the 'Data.Time.Clock.ModJulianDate'
representation of 'Data.Time.Clock.UniversalTime'. This is (semantically)
unsafe in that the time scale of the epoch is not respected. These functions
are only for internal use (i.e., an implementation detail).
-}

-- | Convert an epoch unsafely to the 'Data.Time.Clock.ModJulianDate'
-- representation of 'Data.Time.Clock.UniversalTime'. This is unsafe in
-- the sense that the time scale of the epoch is not respected.
unsafeToModJulianDate :: (Real a, Fractional a) => E t a -> UniversalTime
unsafeToModJulianDate (E t) = ModJulianDate (toRational (t /~ day))
--
-- | Convert an 'Data.Time.Clock.ModJulianDate' unsafely into an epoch.
-- This is unsafe in the sense that the time scale of the epoch is not
-- respected.
unsafeFromModJulianDate :: Fractional a => UniversalTime -> E t a
unsafeFromModJulianDate = mjd' . fromRational . getModJulianDate

-- | Convert an epoch to a representation based on the form of
-- 'Data.Time.LocalTime.LocalTime'. This isn't really semantically
-- accurate: the "locality" actually refers to the time scale in
-- question rather than a geographical longitude. Use with caution!
unsafeToLocalTime :: (Fractional a, Real a) => E t a -> LocalTime
unsafeToLocalTime = ut1ToLocalTime 0 . unsafeToModJulianDate

-- | Convert an 'Data.Time.LocalTime.LocalTime' to an epoch, assuming
-- the same time scale (and zone). This isn't really semantically
-- accurate: the "locality" actually refers to the time scale in
-- question rather than a geographical longitude. Use with caution!
unsafeFromLocalTime :: Fractional a => LocalTime -> E t a
unsafeFromLocalTime = unsafeFromModJulianDate . localTimeToUT1 0


-- * Time Representations
-- ** Clock dates
--    -----------
-- | Define an epoch using "clock time" and time scale. The maximum
-- accuracy is one picosecond which can be obtained e.g. if the type
-- of the parameter for seconds is 'Data.Fixed.Pico' and the numerical
-- representation of the resulting epoch is Rational.
clock :: (Fractional a, Real b)
      => Integer  -- ^ Year
      -> Int      -- ^ Month
      -> Int      -- ^ Day (of month)
      -> Int      -- ^ Hour
      -> Int      -- ^ Minute
      -> b        -- ^ Second, including fraction
      -> t        -- ^ Time scale
      -> E t a    -- ^ Epoch
clock y m d h min s _ = unsafeFromLocalTime $
      LocalTime (fromGregorian y m d) (TimeOfDay h min (realToFrac s))

-- | A version of 'clock' where the time scale isn't an input (it is
-- inferred from the type.
clock' :: (Fractional a, Real b)
       => Integer  -- ^ Year
       -> Int      -- ^ Month
       -> Int      -- ^ Day (of month)
       -> Int      -- ^ Hour
       -> Int      -- ^ Minute
       -> b        -- ^ Second, including fraction
       -> E t a    -- ^ Epoch
clock' y m d h min s = clock y m d h min s undefined

-- | Show an epoch as a clock time. This function is used by the @Show@
-- instance. TODO: should change this to use ISO8601 format.
showClock :: forall t a. (Show t, Show a, Real a, Fractional a) 
          => E t a -> String
showClock t = show (unsafeToLocalTime t) ++ ' ':show (undefined::t)

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
jdEpoch :: Fractional a => E t a
jdEpoch  = subTime mjdEpoch (2400000.5 *~ day)

-- | Define an epoch by specifying a JD and time scale.
jd :: Fractional a => a -> t -> E t a
jd d _ = addTime jdEpoch (d *~ day)

-- | A version of 'jd' where the time scale isn't an input (it is
-- inferred from the type.
jd' :: Fractional a => a -> E t a
jd' d = jd d undefined

-- | Show an epoch as JD on the format \"JD 0.0 TAI\".
showJD :: forall t a. (Show t, Show a, Fractional a) => E t a -> String
showJD e = "JD " ++ show (diffEpoch e jdEpoch /~ day) ++ " " ++ show (undefined::t)


-- ** Modified Julian Date (MJD)
--    --------------------------
{- $mjd
A Modified Julian Date (MJD) is analogous with a Julian Date but with
an initial epoch of 1858-11-17 00:00.
-}

-- | The epoch of MJD 0.0 as an AbsoluteTime.
mjdEpoch :: Num a => E t a
mjdEpoch = E $ 0*~second  -- taiEpoch

-- | Define an epoch by specifying a MJD and time scale.
mjd :: Num a => a -> t -> E t a
mjd d _ = addTime mjdEpoch (d *~ day)

-- | A version of 'mjd' where the time scale isn't an input (it is
-- inferred from the type.
mjd' :: Fractional a => a -> E t a
mjd' d = mjd d undefined

-- | Show an epoch as MJD on the format \"MJD 0.0 TAI\".
showMJD :: forall t a. (Show t, Show a, Fractional a) => E t a -> String
showMJD e = "MJD " ++ show (diffEpoch e mjdEpoch /~ day) ++ " " ++ show (undefined::t)



-- * Terrestrial Time scales
-- =========================
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
{- $tai
For practical applications, International Atomic Time (TAI) is a commonly
used time scale based on the SI second on the Earth's surface at sea level
(specifically, the rotating geoid). TAI is the most precisely determined
time scale that is now available for astronomical use. [C179]
-}
data TAI = TAI; instance Show TAI where show _ = "TAI"


-- | The epoch at which TT, TCG and TDB all read 1977-01-01T00:00:32.184.
convergenceEpochTAI :: Fractional a => E TAI a
convergenceEpochTAI = clock 1977 01 01 00 00 00.000 TAI
convergenceEpochTT :: Fractional a => E TT a
convergenceEpochTT  = clock 1977 01 01 00 00 32.184 TT
convergenceEpochTCG :: Fractional a => E TCG a
convergenceEpochTCG = clock 1977 01 01 00 00 32.184 TCG
convergenceEpochTCB :: Fractional a => E TCB a
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
j2000 :: Fractional a => E TT a
j2000 = clock 2000 01 01 12 00 00.000 TT

-- | The difference in seconds between the given epoch and J2000.0. Will
-- be negative if the given epoch preceeds J2000.0.
sinceJ2000 :: Fractional a => E TT a -> Time a
sinceJ2000 tt = diffEpoch tt j2000

-- | The difference between TAI and TT.
ttMinusTAI :: Fractional a => Time a
ttMinusTAI = 32.184 *~ second  -- (2.4)

-- | Convert a TAI epoch into a TT epoch.
taiToTT :: Fractional a => E TAI a -> E TT a
taiToTT e = coerce $ addTime e ttMinusTAI

-- | Convert a TT epoch into a TAI epoch.
ttToTAI :: Fractional a => E TT a -> E TAI a
ttToTAI e = coerce $ subTime e ttMinusTAI


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
ttMinusTCG :: Fractional a => E TCG a -> Time a
ttMinusTCG tcg = negate l_G * (diffEpoch tcg convergenceEpochTCG)

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
tcgMinusTT :: Fractional a => E TT a -> Time a
tcgMinusTT tt = (tt .- convergenceEpochTT) * (l_G + l_G^pos2)  -- (27) of [dav].

-- | Convert a TCG epoch into a TT epoch.
tcgToTT :: Fractional a => E TCG a -> E TT a
tcgToTT tcg = coerce $ addTime tcg (ttMinusTCG tcg)

-- | Convert a TT epoch into a TCG epoch.
ttToTCG :: Fractional a => E TT a -> E TCG a
ttToTCG tt = coerce $ addTime tt (tcgMinusTT tt)


-- * Barycentric time-scales
--   =======================
{- $bary
The barycentric time scales are Barycentric Coordinate Time (TCB)
and Barycentric Dynamical Time (TDB) where the latter is defined
as a linear function of former.

In terms of implementing TDB the defintion as a linear relation
with TCB is of limited utility in computing TDB from other time
scales as TCB lacks a concrete realization. Instead one would
generally use an (imprecise) expression relating TDB to TT. This
expression, together with the definition of TDB, would then also
be used to obtain TCB.

A concrete implementation of conversions between barycentric and
terrestrial time scales isn't provided in this module. Instead a
module in the @Astro.Time.Barycentric@ namespace should be imported
according to accuracy/performance requirements.
-}

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

The TDB time scale should be used for quantities such as precession
angles and the fundamental arguments. However, for these quantities,
the difference between TDB and TT is negligible at the microarcsecond
level (page B7 of AsA2009).
-}
data TDB = TDB; instance Show TDB where show _ = "TDB"


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
tdb_0 = (-6.55e-5) *~ second

-- | The TDB epoch corresponding to 1977 January 1.0 TAI.
convergenceEpochTDB :: Fractional a => E TDB a
convergenceEpochTDB = coerce $ addTime convergenceEpochTCG tdb_0

coerce :: E t a -> E t' a
coerce (E t) = E t

-- | The difference between the TDB and TCB time scales as a function of
-- TCB epoch.
tdbMinusTCB :: Fractional a => E TCB a -> Time a
tdbMinusTCB tcb = tdb_0 - (tcb .- convergenceEpochTCB) * l_B

{-
'tcbMinusTDB' is defined analogously with 'tcgMinusTT' (see commentary
above). Note that since L_B is two orders of magnitude larger than L_G
the L_B^3 term does have a very small effect.
-}

-- | The difference between the TCB and TDB time scales as a function of
-- TDB epoch.
tcbMinusTDB :: Fractional a => E TDB a -> Time a
tcbMinusTDB tdb = (tdb .- convergenceEpochTDB) * (l_B + l_B^pos2 + l_B^pos3) - tdb_0

-- | Convert a TCB epoch into a TDB epoch.
tcbToTDB :: Fractional a =>  E TCB a -> E TDB a
tcbToTDB tcb@(E t) = E $ t + (tdbMinusTCB tcb)

-- | Convert a TDB epoch into a TCB epoch.
tdbToTCB :: Fractional a => E TDB a -> E TCB a
tdbToTCB tdb@(E t) = E $ t + (tcbMinusTDB tdb)


-- Universal Time
-- ==============

data UT1 = UT1; instance Show UT1 where show _ = "UT1"

-- | Function which for a given TAI epoch calculates the instantaneous
-- difference @UT1-TAI@.
type UT1MinusTAI a = E TAI a -> Time a
-- | Function which for a given UT1 epoch calculates the instantaneous
-- difference @TAI-UT1@. TODO potentially unnecessary?
type TAIMinusUT1 a = E UT1 a -> Time a

-- | Convert a UT1 epoch into a TAI epoch.
ut1ToTAI :: Num a => TAIMinusUT1 a -> E UT1 a -> E TAI a
ut1ToTAI f ut1@(E t) = E $ t + f ut1

-- | Convert a TAI epoch into a UT1 epoch.
taiToUT1 :: Num a => UT1MinusTAI a -> E TAI a -> E UT1 a
taiToUT1 f tai@(E t) = E $ t + f tai


-- References
-- ==========
{-$ref

 * [C179] <http://aa.usno.navy.mil/publications/docs/Circular_179.php>

 * [B3]   <http://www.iau.org/static/resolutions/IAU2006_Resol3.pdf>

 * [dav]  <http://www.centerforspace.com/downloads/files/pubs/AAS-06-134.pdf>

 * [AAG]  <http://asa.usno.navy.mil/SecM/Glossary.html>
 
 * [WPJD] <http://en.wikipedia.org/wiki/Julian_date>

-}

