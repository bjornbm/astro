{-# LANGUAGE FlexibleContexts #-}

module Astro.Celestrak where

import Numeric.Units.Dimensional.Prelude
import Astro.Time
import Astro.Time.Interop
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Data.Ratio
import Data.List (isPrefixOf)
import Data.Time
import Data.Time.Clock.TAI
import qualified Prelude as P
import Data.Array.IArray


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
    readTime     = (*~ second) . read
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


-- | Creates a 'Data.Time.Clock.TAI.LeapSecondMap' from an 'EOPArray'.
mkLeapSecondMap :: EOPArray a -> LeapSecondMap
mkLeapSecondMap a d = Just . fromIntegral
  $ if d <= i then get i else if d >= j then get j else get d
  -- TODO Is this sensible, or should Nothing be returned outside bounds?
  where
    (i,j) = bounds a
    get = deltaAT . (a!)


-- | Returns the UTC day that the epoch occurs on.
getUTCDay :: (Real a, Fractional a) => LeapSecondMap -> E TAI a -> Day
getUTCDay lst = utctDay . fromJust . taiToUTCTime lst . toAbsoluteTime
                  -- TODO ^^^^^^^^ error handling or fail in Astro?

-- | Creates a 'UT1Table' from an 'EOPArray'.
{-
TODO: The IERS explanatory supplement
<http://hpiers.obspm.fr/iers/bul/bulb/explanatory.html> says:

  "There are short-periodic (diurnal, semi-diurnal) variations in UT1 due
  to ocean tides that are treated similarly to polar motion (the IERS
  publishes the daily values from which these terms have been removed,
  and they are to be added back after the interpolation)."

Suitable interpolation method and addition of tides are described at
<http://maia.usno.navy.mil/iers-gaz13>.
-}
mkUT1Table :: (Real a, Fractional a) => EOPArray a -> UT1MinusTAI a
mkUT1Table a t = if d < i then get i else if d >= j then get j
  else interpolate (t0, get d) (t1, get $ succ d) t
  where
    lst = mkLeapSecondMap a
    (i,j) = bounds a
    d = getUTCDay lst t
    t0 = utcDayToTAI d
    t1 = utcDayToTAI (succ d)
    utcDayToTAI d = fromAbsoluteTime $ fromJust $ utcToTAITime lst (UTCTime d 0)
                               -- TODO ^^^^^^^^ error handling or fail in Astro?
    get n = ut1MinusUTC (a!n) - fromInteger (deltaAT (a!n)) *~ second


-- The following are subject to extraction to a util module if they
-- turn out to be useful elsewhere.

{-
Linear interpolation is obviously simplistic. Another method should be
used, see e.g. <http://maia.usno.navy.mil/iers-gaz13>. This recommendation
is from 1997. Verify that it is still relevant for IERS2003... perhaps
the CSSI paper on EOP addresses this? Or AsA2009?
-}

-- | Linear interpolation between two points.
interpolate :: Fractional a => (E t a, Quantity d a) -> (E t a, Quantity d a) -> E t a -> Quantity d a
interpolate (t0, x0) (t1, x1) t = (t .- t0) / (t1 .- t0) * (x1 - x0) + x0
  where (.-) = diffEpoch
