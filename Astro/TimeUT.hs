{-# LANGUAGE FlexibleContexts
  #-}
module Astro.TimeUT where

import Numeric.Units.Dimensional.Prelude
import Astro.TimeE
import Data.Fixed (Pico)
import Data.Time
import Data.Time.Clock.TAI
import qualified Prelude


-- UTC
-- ===
-- Just a bunch of wrappers for conversions. Use the regular UTCTime data
-- type and 'NominalDiffTime' et al for arithmetic.

utcToTAI :: LeapSecondTable -> UTCTime -> E TAI
utcToTAI lst = wrapTAI . utcToTAITime lst

taiToUTC :: LeapSecondTable -> E TAI -> UTCTime
taiToUTC lst = taiToUTCTime lst . unwrapTAI

convertToUTC :: Convert t TAI => LeapSecondTable -> E t -> UTCTime
convertToUTC lst = taiToUTC lst . convert

convertFromUTC :: Convert TAI t => LeapSecondTable -> UTCTime -> E t
convertFromUTC lst = convert . utcToTAI lst


-- UT1
-- ===

data UT1 = UT1
type UT1MinusTAI = E TAI -> Time Pico
type TAIMinusUT1 = E UT1 -> Time Pico

type UT1Table = UTCTime -> Time Pico

ut1ToTAI :: TAIMinusUT1 -> E UT1 -> E TAI
ut1ToTAI f ut1@(E t) = E $ t `addTime` f ut1

taiToUT1 :: UT1MinusTAI -> E TAI -> E UT1
taiToUT1 f tai@(E t) = E $ t `addTime` f tai

convertToUT1 :: Convert t TAI => UT1MinusTAI -> E t -> E UT1
convertToUT1 f = taiToUT1 f . convert

convertFromUT1 :: Convert TAI t => TAIMinusUT1 -> E UT1 -> E t
convertFromUT1 f = convert . ut1ToTAI f

makeUT1Tables :: LeapSecondTable -> UT1Table -> (UT1MinusTAI, TAIMinusUT1)
makeUT1Tables = undefined

