{-# LANGUAGE ScopedTypeVariables #-}

module Astro.Time.Format where

import Astro.Time
import Data.Time
import System.Locale


-- Formatting
-- ==========

-- | Format an epoch using the formatting codes of
-- 'Data.Time.Format.formatTime'. Some specifics to keep in mind:
--
--   *  Week days will in general not make sense other than for UT1.
--
--   *  The time zone name (%Z) will be the time scale name.
--
--   *  The time zone offset (%z) will always be 00:00.
--
formatEpoch :: forall t a. (Show t, RealFrac a)
            => TimeLocale -> String -> E t a -> String
formatEpoch locale format e = formatTime locale format
                            $ ZonedTime (unsafeToLocalTime e) tz
  where tz = TimeZone 0 False $ show (undefined :: t)

-- | Same as 'formatEpoch' but uses a sensible (English, ISO8601)
-- 'System.Locale.TimeLocale'.
formatEpoch' :: (Show t, RealFrac a) => String -> E t a -> String
formatEpoch' = formatEpoch locale
  where
    locale = defaultTimeLocale { dateTimeFmt = "%FT%T%Q %Z"
                               , dateFmt = "%F"
                               , timeFmt = "%T%Q"
                               }
