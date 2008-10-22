{-# LANGUAGE FlexibleContexts
  #-}
module Astro.TimeUT where

import Numeric.Units.Dimensional.Prelude
import Astro.Time
import Data.Fixed (Pico)
import Data.Time
import Data.Time.Clock.TAI
import qualified Prelude
import Data.Ix
import Data.Array.IArray


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


-- Celestrak
-- =========
-- | Datatype holding all the parameters given in Celestrak EOP data file
-- for a single day.
data EOPData a = EOPData
  { x           :: Angle a
  , y           :: Angle a
  , ut1MinusUTC :: Time a
  , lod         :: Time a
  , dPsi        :: Angle a
  , dEpsilon    :: Angle a
  , dX          :: Angle a
  , dY          :: Angle a
  , deltaAT     :: Integer
  } deriving (Show, Eq)

-- | Aggregates of the above indexed/coupled with 'Day's.
type EOPList  a = [(Day, EOPData a)]
type EOPArray a = Array Day (EOPData a)

-- | Parses a single line of EOP data.
parseEOPLine :: (Read a, Floating a) => String -> (Day, EOPData a)
parseEOPLine s = ( ModifiedJulianDay (read mjd)
                 , EOPData (readAngle x) (readAngle y)
                           (readTime ut1MinusUTC) (readTime lod)
                           (readAngle dPsi) (readAngle dEpsilon)
                           (readAngle dX) (readAngle dY)
                           (read deltaAT)
                 )
  where
    [_, _, _, mjd, x, y, ut1MinusUTC, lod, dPsi, dEpsilon, dX, dY, deltaAT] = words s
    readTime  = (*~ second) . read
    readAngle = (*~ arcsecond) . read

-- | Parse a file of Celestrak EOP data into a list.
parseEOPData :: (Read a, Floating a) => String -> EOPList a
parseEOPData file = map parseEOPLine (observed ++ predicted)
  where
    a = lines file
    b = tail $ dropWhile  (/= "BEGIN OBSERVED\r")  a
    observed = takeWhile  (/= "END OBSERVED\r")    b
    c = tail $ dropWhile  (/= "BEGIN PREDICTED\r") b
    predicted = takeWhile (/= "END PREDICTED\r")   c

-- | Creates and EOPArray from an EOPList. Assumes that the EOPList is
-- complete, i.e. there is one element per day and that the first and last
-- elements bound the days.
mkEOPArray :: EOPList a -> EOPArray a
mkEOPArray table = array (fst $ head table, fst $ last table) table


-- | Creates a 'Data.Time.Clock.TAI.LeapSecondTable' from an EOPArray.
mkLeapSecondTable :: EOPArray a -> LeapSecondTable
mkLeapSecondTable a d = deltaAT eop
  where
    b@(i,j) = bounds a
    eop = if d < i then a ! i else if d > j then a ! j else a ! d



-- Methinks this should be part of the time package... submit patch!
instance Ix Day where
  range (d1, d2) = map ModifiedJulianDay $ range (toModifiedJulianDay d1, toModifiedJulianDay d2)
  index (d1, d2) d = index (toModifiedJulianDay d1, toModifiedJulianDay d2) (toModifiedJulianDay d)
  inRange (d1, d2) d = inRange (toModifiedJulianDay d1, toModifiedJulianDay d2) (toModifiedJulianDay d)
  rangeSize (d1, d2) = rangeSize (toModifiedJulianDay d1, toModifiedJulianDay d2)

