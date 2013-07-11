{-# LANGUAGE RecordWildCards #-}

module TLE.Util where

import Astrodynamics
import TLE
import TLE.Parse
import Astro.Util
import Astro.Time
import Safe (fromJustNote)
import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Data.Attoparsec.Text (parse, maybeResult)
import qualified Data.Text as T (pack)

-- | Parse a three line string.
parseTLE' :: String -> TLE Double
parseTLE' = fromJustNote "Bad TLE!" . maybeResult . parse parseTLE . T.pack 

-- | Provide the mean longitude of the object at the TLE epoch.
meanLongitudeAtEpoch :: RealFloat a => TLE a -> Angle a
meanLongitudeAtEpoch TLE {..} = zeroTwoPi
                              $ raToLongitude (coerceUtcToUT1 epoch)
                              $ raan + aop + meanAnomaly

-- | Provide the mean longitude of the object at any epoch by taking into
-- account the Mean Motion and the Ballistic Coefficient (dMdt).
meanLongitude :: RealFloat a => E UT1 a -> TLE a -> Angle a
meanLongitude t tle@TLE {..} = zeroTwoPi
    $ raToLongitude t
    $ raan + aop + meanAnomaly
    + dt * (meanMotion + dMdt * dt + d2Mdt2 * dt ^ pos2)
  where
    dt = t `diffEpoch` coerceUtcToUT1 epoch
