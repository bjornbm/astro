-- This sorry module is a holdall for small useful stuff while it waits
-- for a more appropriate place to live.
module Astro.Util ( r_GEO  -- From Astrodynamics
                  , perfectGEO, perfectGEO'
                  , coerceUt1ToUTC, coerceUtcToUT1
                  ) where

import Astro.Coords
import Astro.Coords.PosVel
import Astro.Time
import Astro.Time.Interop
import Astrodynamics (r_GEO)
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra (fromTuple)

import Data.Time.Clock
import Data.Time.Clock.TAI
import qualified Prelude


type Longitude = Angle

-- | ECEF(??) position of a perfectly geostationary SC.
perfectGEO :: Floating a => Longitude a -> Coord ECR a
perfectGEO l = S $ fromTuple (r_GEO, 90*~degree, l)

-- | ECEF posvel of a perfectly geostationary SC.
perfectGEO' :: RealFloat a => Longitude a -> PosVel ECR a
perfectGEO' l = S' (s $ perfectGEO l)
                   (fromTuple (_0, _0, _0))

-- | Coerces a UT1 epoch to a `UTCTime`. Note that this is an approximation
-- that is only accurate to within 0.9 s. Use with care!
coerceUt1ToUTC :: RealFloat a => E UT1 a -> UTCTime
coerceUt1ToUTC (E t) = taiToUTCTime (const 0) $ toAbsoluteTime (E t)

-- | Coerces a UT1 epoch to a `UTCTime`. Note that this is an approximation
-- that is only accurate to within 0.9 s. Use with care!
coerceUtcToUT1 :: RealFloat a => UTCTime -> E UT1 a
coerceUtcToUT1 = coerce . fromAbsoluteTime . utcToTAITime (const 0)
  where coerce (E t) = E t

