
> import Data.Fixed
> import Data.Time.Clock
> import Data.Time.Clock.TAI
> import Data.Time.Format
> import System.Locale
> import Test.QuickCheck

> import Time

QuickCheck properties
=====================

First we define an 'Arbitrary' instance for 'DiffTime'.

> instance Arbitrary DiffTime where
>   arbitrary   = fmap realToFrac (arbitrary :: Gen Float)
>   coarbitrary = undefined -- To avoid compiler warnings

And now the properties.

> prop_taiTT, prop_ttTAI, prop_taiTTDiff :: DiffTime -> Bool

Conversions to another time system and back does not change the time.

> prop_taiTT dtai = ttToTAITime (taiToTTTime tai) == tai
>   where tai = addAbsoluteTime dtai taiEpoch
> prop_ttTAI dtt  = taiToTTTime (ttToTAITime tt)  == tt
>   where tt  = MkTerrestrialTime dtt

The relative difference between TAI and TT is constant.

> prop_taiTTDiff dt = diffAbsoluteTime tai (ttToTAITime tt) == 32.184
>   where
>       tai = addAbsoluteTime dt taiEpoch
>       tt  = MkTerrestrialTime dt
> prop_ttTAIDiff dt = diffTerrestrialTime tt (taiToTTTime tai) == -32.184
>   where
>       tai = addAbsoluteTime dt taiEpoch
>       tt  = MkTerrestrialTime dt

Reading, parsing and Showing
----------------------------

> prop_readShowTT = show (read a::TerrestrialTime) == a ++ " TT"
>   where a = "2007-03-22 12:22:22.212"

> prop_parseShowTT = show (readTime defaultTimeLocale "%F %T%Q TT" a :: TerrestrialTime) == a
>   where a = "2007-09-25 11:23:43.21 TT"

> prop_readShowTAI = show (read a::AbsoluteTime) == a ++ " TAI"
>   where a = "2007-03-22 12:22:22.212"

> prop_parseShowTAI = show (readTime defaultTimeLocale "%F %T%Q TAI" a :: AbsoluteTime) == a
>   where a = "2007-09-25 11:23:43.21 TAI"

Main function
=============

Just tests QuickCheck properties.

> main = do
>   quickCheck prop_taiTT
>   quickCheck prop_ttTAI
>   quickCheck prop_taiTTDiff
>   quickCheck prop_ttTAIDiff
>   oneCheck prop_readShowTT
>   oneCheck prop_parseShowTT
>   oneCheck prop_readShowTAI
>   oneCheck prop_parseShowTAI
>   where oneCheck = check (defaultConfig {configMaxTest = 1})

