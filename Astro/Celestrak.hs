{-# LANGUAGE FlexibleContexts #-}

module Astro.Celestrak where

import Numeric.Units.Dimensional.Prelude
import Astro.Time
import Astro.Time.Interop
import Data.Fixed (HasResolution, Fixed, Micro, Pico)
import Data.Char (isSpace)
import Data.Ratio
import Data.List (isPrefixOf)
import Data.Time
import Data.Time.Clock.TAI
import qualified Prelude as P
import Data.Ix
import Data.Array.IArray


-- Celestrak
-- =========
-- | Datatype holding all the parameters given in Celestrak EOP data file
-- for a single day.
data EOPData a = EOPData
  { x           :: Angle a
  , y           :: Angle a
  , ut1MinusUTC :: Time Pico
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
                           (readPicoTime ut1MinusUTC) (readTime lod)
                           (readAngle dPsi) (readAngle dEpsilon)
                           (readAngle dX) (readAngle dY)
                           (read deltaAT)
                 )
  where
    [_, _, _, mjd, x, y, ut1MinusUTC, lod, dPsi, dEpsilon, dX, dY, deltaAT] = words s
    readTime     = (*~ second) . read
    readPicoTime = (*~ second) . readFixed -- fromRational . toRational . (read :: String -> Double)
    readAngle    = (*~ arcsecond) . read

-- | Parse a file of Celestrak EOP data into a list.
parseEOPData :: (Read a, Floating a) => String -> EOPList a
parseEOPData file = map parseEOPLine (observed ++ predicted)
  where
    a = lines file
    b = tail $ dropWhile  (not . isPrefixOf "BEGIN OBSERVED")  a
    observed = takeWhile  (not . isPrefixOf "END OBSERVED")    b
    c = tail $ dropWhile  (not . isPrefixOf "BEGIN PREDICTED") b
    predicted = takeWhile (not . isPrefixOf "END PREDICTED")   c

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
getUTCDay :: LeapSecondTable -> E TAI -> Day
getUTCDay lst = utctDay . taiToUTCTime lst . toAbsoluteTime

-- | Creates a 'UT1Table' from an 'EOPArray'.
mkUT1Table :: (Fractional a) => EOPArray a -> UT1MinusTAI
mkUT1Table a t = if d < i then get i else if d >= j then get j
  else interpolate (t0, get d) (t1, get $ succ d) t
  where
    lst = mkLeapSecondTable a
    (i,j) = bounds a
    d = getUTCDay lst t
    t0 = utcDayToTAI d
    t1 = utcDayToTAI (succ d)
    utcDayToTAI d = fromAbsoluteTime $ utcToTAITime lst (UTCTime d 0)
    get n = ut1MinusUTC (a!n) - fromInteger (deltaAT (a!n)) *~ second


-- The following are subject to extraction to a util module if they
-- turn out to be useful elsewhere.

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

-- | Read a 'Data.Fixed.Fixed' value. Note that any decimals beyond the
-- resolution of the target type are "floored" (as opposed to rounded).
readFixed :: HasResolution a => String -> Fixed a
readFixed s = fromRational $ int % (10 P.^ decimals)
    where
      int      = read $ filter (/='.') s
      decimals = if elem '.' s then length $ takeWhile (/='.') $ dropWhile isSpace $ reverse s else 0
      --decimals = length $ takeWhile (/='.') $ dropWhile isSpace $ reverse s

