-- This sorry module is a holdall for small useful stuff while it waits
-- for a more appropriate place to live.
module Astro.Util ( r_GEO  -- From Astrodynamics
                  , perfectGEO, perfectGEO'
                  , coerceUt1ToUTC, coerceUtcToUT1
                  , normalizeAngle, plusMinusPi, zeroTwoPi, fractionalPart
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


-- Perfect GEO vectors
-- -------------------

-- | ECEF(??) position of a perfectly geostationary SC.
perfectGEO :: Floating a => Longitude a -> Coord ECR a
perfectGEO l = S $ fromTuple (r_GEO, 90*~degree, l)

-- | ECEF posvel of a perfectly geostationary SC.
perfectGEO' :: RealFloat a => Longitude a -> PosVel ECR a
perfectGEO' l = S' (s $ perfectGEO l)
                   (fromTuple (_0, _0, _0))


-- Approximating UTC == UT1
-- ------------------------

-- | Coerces a UT1 epoch to a `UTCTime`. Note that this is an approximation
-- that is only accurate to within 0.9 s. Use with care!
coerceUt1ToUTC :: RealFloat a => E UT1 a -> UTCTime
coerceUt1ToUTC (E t) = taiToUTCTime (const 0) $ toAbsoluteTime (E t)

-- | Coerces a UT1 epoch to a `UTCTime`. Note that this is an approximation
-- that is only accurate to within 0.9 s. Use with care!
coerceUtcToUT1 :: RealFloat a => UTCTime -> E UT1 a
coerceUtcToUT1 = coerce . fromAbsoluteTime . utcToTAITime (const 0)
  where coerce (E t) = E t


-- Dealing with cyclic angles
-- -------------------------

-- | @normalizeAngle center a@ normalizes the angle @a@ to
-- be within ±π of @center@. Algorithm from
-- <http://www.java2s.com/Tutorial/Java/0120__Development/Normalizeanangleina2piwideintervalaroundacentervalue.htm>.
normalizeAngle :: RealFloat a => Angle a -> Angle a -> Angle a
normalizeAngle center a = a - _2 * pi * floor' ((a + pi - center) / (_2 * pi))
  where floor' = (*~ one) . fromIntegral . floor . (/~ one)

-- | Constrains an angle to the range [-pi,pi).
plusMinusPi :: RealFloat a => Angle a -> Angle a
plusMinusPi = normalizeAngle _0
-- | Constrains an angle to the range [0,2*pi).
zeroTwoPi   :: RealFloat a => Angle a -> Angle a
zeroTwoPi   = normalizeAngle pi

-- | Removes the integral part of a value so that it ends up in the
-- interval [0,1).
fractionalPart :: RealFrac a => Dimensionless a -> Dimensionless a
fractionalPart x = x - fromIntegral (floor (x /~ one)) *~ one
