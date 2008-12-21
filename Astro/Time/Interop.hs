{-# LANGUAGE FlexibleContexts #-}

-- | This module provides functions for interoperability with the time
-- library ("Data.Time" module hierarchy).
module Astro.Time.Interop where

import Astro.Time
import Data.Time
import Data.Time.Clock.TAI
import Data.Fixed (Pico)


-- | Converta TAI epoch into a 'Data.Time.Clock.TAI.AbsoluteTime'.
toAbsoluteTime :: E TAI -> AbsoluteTime
toAbsoluteTime (E t) = t

-- | Converts a 'Data.Time.Clock.TAI.AbsoluteTime' into a TAI epoch.
fromAbsoluteTime :: AbsoluteTime -> E TAI
fromAbsoluteTime = E

-- | Convert a UT1 epoch into a 'Data.Time.Clock.UniversalTime'.
toUniversalTime :: E UT1 -> UniversalTime
toUniversalTime (E t) = ModJulianDate $ (/ 86400) $ toRational $ diffAbsoluteTime t taiEpoch

-- | Convert a 'Data.Time.Clock.UniversalTime' into a UT1 epoch.
fromUniversalTime :: UniversalTime -> E UT1
fromUniversalTime t = mjd (getModJulianDate t) UT1


-- UTC
-- ===
-- We do not define a UTC time scale since UTC is generally used in
-- astrodynamics algorithms. Typically UTC is only used for inputs
-- or outputs but immediately converted to another time scale. For
-- UTC epochs use 'Data.Time.Clock.UTCTime'.

clockUTC :: Integer  -- ^ Year
         -> Int      -- ^ Month
         -> Int      -- ^ Day (of month)
         -> Int      -- ^ Hour
         -> Int      -- ^ Minute
         -> Pico     -- ^ Second, including fraction
         -> UTCTime  -- ^ Epoch
clockUTC y m d h min s = UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay h min s)


utcToTAI :: LeapSecondTable -> UTCTime -> E TAI
utcToTAI lst = fromAbsoluteTime . utcToTAITime lst

taiToUTC :: LeapSecondTable -> E TAI -> UTCTime
taiToUTC lst = taiToUTCTime lst . toAbsoluteTime


convertToUTC :: Convert t TAI => LeapSecondTable -> E t -> UTCTime
convertToUTC lst = taiToUTC lst . convert

convertFromUTC :: Convert TAI t => LeapSecondTable -> UTCTime -> E t
convertFromUTC lst = convert . utcToTAI lst


