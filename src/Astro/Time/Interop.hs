{-# LANGUAGE FlexibleContexts #-}

-- | This module provides functions for interoperability with the time
-- library ("Data.Time" module hierarchy).
module Astro.Time.Interop where

import Astro
import Astro.Time
import Astro.Time.Convert
import Data.Time
import Data.Time.Clock.TAI
import Control.Monad.Reader (asks)
import Numeric.Units.Dimensional.Prelude hiding (toDiffTime, fromDiffTime)
import Numeric.Units.Dimensional.DK.Time


-- | Convert a TAI epoch into a 'Data.Time.Clock.TAI.AbsoluteTime'.
toAbsoluteTime :: (Real a, Fractional a) => E TAI a -> AbsoluteTime
toAbsoluteTime (E t) = addAbsoluteTime (toDiffTime t) taiEpoch

-- | Convert a 'Data.Time.Clock.TAI.AbsoluteTime' into a TAI epoch.
fromAbsoluteTime :: Fractional a => AbsoluteTime -> E TAI a
fromAbsoluteTime t = E $ fromDiffTime $ diffAbsoluteTime t taiEpoch

-- | Convert a UT1 epoch into a 'Data.Time.Clock.UniversalTime'.
toUniversalTime :: (Real a, Fractional a) => E UT1 a -> UniversalTime
toUniversalTime = unsafeToModJulianDate

-- | Convert a 'Data.Time.Clock.UniversalTime' into a UT1 epoch.
fromUniversalTime :: Fractional a => UniversalTime -> E UT1 a
fromUniversalTime = unsafeFromModJulianDate


-- UTC
-- ===
-- We do not define a UTC time scale since UTC is generally not used
-- in astrodynamics algorithms. Typically UTC is only used for inputs
-- or outputs but immediately converted to another time scale. For
-- UTC epochs use 'Data.Time.Clock.UTCTime'.

-- | Convenience function for specifying UTC epochs.
clockUTC :: Real a
         => Integer  -- ^ Year
         -> Int      -- ^ Month
         -> Int      -- ^ Day (of month)
         -> Int      -- ^ Hour
         -> Int      -- ^ Minute
         -> a        -- ^ Second, including fraction
         -> UTCTime  -- ^ Epoch
clockUTC y m d h min s = UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay h min (realToFrac s))

-- Conversion
-- ----------
-- | Conversion from UTCTime.
utcToTAI :: Fractional a => LeapSecondTable -> UTCTime -> E TAI a
utcToTAI lst = fromAbsoluteTime . utcToTAITime lst

-- | Conversion to UTCTime.
taiToUTC :: (Real a, Fractional a) => LeapSecondTable -> E TAI a -> UTCTime
taiToUTC lst = taiToUTCTime lst . toAbsoluteTime

-- | Monadic conversion to UTCTime.
convertToUTC :: (Convert t TAI, Real a, Fractional a) => E t a -> Astro a UTCTime
convertToUTC t = do
  lst <- asks (leapSecondTable.time)
  tai <- convert t
  return (taiToUTC lst tai)

-- | Monadic conversion from UTCTime.
convertFromUTC :: (Convert TAI t, Fractional a) => UTCTime -> Astro a (E t a)
convertFromUTC utc = do
  lst <- asks (leapSecondTable.time)
  convert (utcToTAI lst utc)
