-- | This module provides functions for interoperability with the time
-- library ("Data.Time" module hierarchy).
module Astro.Time.Interop where

import Astro.Time
import Data.Time
import Data.Time.Clock.TAI


-- | Converta TAI epoch into a 'Data.Time.Clock.TAI.AbsoluteTime'.
toAbsoluteTime :: E TAI -> AbsoluteTime
toAbsoluteTime (E t) = t

-- | Converts a 'Data.Time.Clock.TAI.AbsoluteTime' into a TAI epoch.
fromAbsoluteTime :: AbsoluteTime -> E TAI
fromAbsoluteTime = E

-- | Convert a UTC epoch into a 'Data.Time.Clock.UTCTime'.
toUTCTime :: E UTC -> UTCTime
toUTCTime (E t) = taiToUTCTime (const 0) t

-- | Convert a 'Data.Time.Clock.UTCTime' into a UTC epoch.
fromUTCTime :: UTCTime -> E UTC
fromUTCTime = E . utcToTAITime (const 0)

-- | Convert a UT1 epoch into a 'Data.Time.Clock.UniversalTime'.
toUniversalTime :: E UT1 -> UniversalTime
toUniversalTime (E t) = ModJulianDate $ (/ 86400) $ toRational $ diffAbsoluteTime t taiEpoch

-- | Convert a 'Data.Time.Clock.UniversalTime' into a UT1 epoch.
fromUniversalTime :: UniversalTime -> E UT1
fromUniversalTime t = mjd (getModJulianDate t) UT1

