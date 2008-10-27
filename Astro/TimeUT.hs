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

-- | Function which for a given TAI epoch calculates the instantaneous
-- difference @UT1-TAI@.
type UT1Table a = E TAI -> Time a

ut1ToTAI :: TAIMinusUT1 -> E UT1 -> E TAI
ut1ToTAI f ut1@(E t) = E $ t `addTime` f ut1

taiToUT1 :: UT1MinusTAI -> E TAI -> E UT1
taiToUT1 f tai@(E t) = E $ t `addTime` f tai

convertToUT1 :: Convert t TAI => UT1MinusTAI -> E t -> E UT1
convertToUT1 f = taiToUT1 f . convert

convertFromUT1 :: Convert TAI t => TAIMinusUT1 -> E UT1 -> E t
convertFromUT1 f = convert . ut1ToTAI f

makeUT1Tables :: LeapSecondTable -> UT1Table a -> (UT1MinusTAI, TAIMinusUT1)
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


-- | Creates a 'Data.Time.Clock.TAI.LeapSecondTable' from an 'EOPArray'.
mkLeapSecondTable :: EOPArray a -> LeapSecondTable
mkLeapSecondTable a d = if d <= i then get i else if d >= j then get j else get d
  where
    (i,j) = bounds a
    get = deltaAT . (a!)


-- | Returns the UTC day that the epoch occurs on.
getUTCDay :: Convert t TAI => LeapSecondTable -> E t -> Day
getUTCDay lst = utctDay . convertToUTC lst

-- | Creates a 'UT1Table' from an 'EOPArray'.
mkUT1Table :: (Fractional a) => EOPArray a -> UT1Table a
mkUT1Table a t = if d < i then get i else if d >= j then get j
  else interpolate (t0, get d) (t1, get $ succ d) t
  where
    lst = mkLeapSecondTable a
    (i,j) = bounds a
    d = getUTCDay lst t
    t0 = utcDayToTAI d
    t1 = utcDayToTAI (succ d)
    utcDayToTAI d = convertFromUTC lst (UTCTime d 0) :: E TAI
    get n = ut1MinusUTC (a!n) - fromInteger (deltaAT (a!n)) *~ second

-- | Linear interpolation between two points.
interpolate :: (Mul DOne d d, Fractional a) => (E t, Quantity d a) -> (E t, Quantity d a) -> E t -> Quantity d a
interpolate (t0, x0) (t1, x1) t = (t .- t0) / (t1 .- t0) * (x1 - x0) + x0
  where (.-) = diffEpoch



-- Methinks this should be part of the time package... submit patch!
instance Ix Day where
  range (d1, d2) = map ModifiedJulianDay $ range (toModifiedJulianDay d1, toModifiedJulianDay d2)
  index (d1, d2) d = index (toModifiedJulianDay d1, toModifiedJulianDay d2) (toModifiedJulianDay d)
  inRange (d1, d2) d = inRange (toModifiedJulianDay d1, toModifiedJulianDay d2) (toModifiedJulianDay d)
  rangeSize (d1, d2) = rangeSize (toModifiedJulianDay d1, toModifiedJulianDay d2)

