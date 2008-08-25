
Various time systems relevant to astrodynamics applications.

> module Time
>   where

> import Data.Time.Clock
> import Data.Time.Clock.TAI
> import Data.Time.Format
> import Data.Time.LocalTime
> -- import System.Locale

Terrestrial Time (TT)
=====================
Terrestrial Time (TT) is a theoretically ideal time advancing at
the same rate as TAI.

> newtype TerrestrialTime = MkTerrestrialTime { unTerrestrialTime :: DiffTime } deriving Eq

Terrestrial Time is practically realized as
    TT = TAI + 32.184 s .
We'll use the difference between TT and TAI a few times so we define a constant for it.

> ttMinusTAI = 32.184


Conversions
-----------
Conversion between TAI and TT.

> taiToTTTime :: AbsoluteTime -> TerrestrialTime
> taiToTTTime tai = MkTerrestrialTime (dtai + ttMinusTAI) where
>   dtai = diffAbsoluteTime tai taiEpoch -- Necessary to "unwrap" the DiffTime.

> ttToTAITime :: TerrestrialTime -> AbsoluteTime
> ttToTAITime (MkTerrestrialTime dtt) = addAbsoluteTime (dtt - ttMinusTAI) taiEpoch

Conversion between UTC and TT for convenience.

> utcToTTTime :: LeapSecondTable -> UTCTime -> TerrestrialTime
> utcToTTTime table = taiToTTTime . utcToTAITime table

> ttToUTCTime :: LeapSecondTable -> TerrestrialTime -> UTCTime
> ttToUTCTime table = taiToUTCTime table . ttToTAITime


DiffTimes
---------
addTerrestrialTime a b = a + b

> addTerrestrialTime :: DiffTime -> TerrestrialTime -> TerrestrialTime
> addTerrestrialTime t (MkTerrestrialTime a) = MkTerrestrialTime (a + t)

diffTerrestrialTime a b = a - b

> diffTerrestrialTime :: TerrestrialTime -> TerrestrialTime -> DiffTime
> diffTerrestrialTime (MkTerrestrialTime a) (MkTerrestrialTime b) = a - b



Test values

> tai1  = addAbsoluteTime 86400 taiEpoch
> tt1   = taiToTTTime tai1
> tai1' = ttToTAITime tt1


Showing, Parsing and reading
============================

Absolute Time (TAI)
-------------------
Free-riding on the instances for UTCTime in Data.Time.Format.Parse.

> instance ParseTime AbsoluteTime where
>   buildTime l = coerceUTCToTAI . buildTime l

> instance Read AbsoluteTime where
>   readsPrec n = mapFst coerceUTCToTAI . readsPrec n


Terrestrial Time (TT)
---------------------
This show instance is modeled after that for TAI.

> instance Show TerrestrialTime where
>   show tt = (show . utcToLocalTime utc . coerceTAIToUTC . coerceTTToTAI) tt ++ " TT"

> instance ParseTime TerrestrialTime where
>   buildTime l = coerceTAIToTT . buildTime l

> instance Read TerrestrialTime where
>   readsPrec n = mapFst coerceTAIToTT . readsPrec n


Coersion and helpers
--------------------

> mapFst f = map (\(x,y) -> (f x, y))
> coerceUTCToTAI = utcToTAITime (const 0)
> coerceTAIToUTC = taiToUTCTime (const 0)
> coerceTAIToTT tai = MkTerrestrialTime (diffAbsoluteTime tai taiEpoch)
> coerceTTToTAI tt  = addAbsoluteTime (unTerrestrialTime tt) taiEpoch


> {-

A 'TimeLocale' for ISO8601 formatting of dates and times. ISO8601
has no 12 hour format and so this locale will use a 24 hour
format even when 12 hour format is requested. 

> iso8601TimeLocale = defaultTimeLocale {
>   dateTimeFmt = "%FT%T%z",
>   dateFmt     = "%F",
>   timeFmt     = "%T",
>   time12fmt   = "%T"  -- No 12 hour format in ISO8601.
>   provide a 
>   }
> -}


