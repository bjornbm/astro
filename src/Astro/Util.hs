-- This sorry module is a holdall for small useful stuff while it waits
-- for a more appropriate place to live.
module Astro.Util ( r_GEO  -- From Astrodynamics
                  , perfectGEO, perfectGEO'
                  , coerceUt1ToUTC, coerceUtcToUT1
                  , timeAngle, sexagesimalAngle, dmsAngle
                  ) where

import Astro.Coords
import Astro.Coords.PosVel
import Astro.Time
import Astro.Time.Interop
import Astrodynamics (r_GEO)
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (revolution)
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.UnitNames (atom)

import Data.Time.Clock
import Data.Time.Clock.TAI
import qualified Prelude


type Longitude = Angle


-- Perfect GEO vectors
-- -------------------

-- | ECEF position of a perfectly geostationary SC.
perfectGEO :: Floating a => Longitude a -> Coord ECR a
perfectGEO l = C (r_GEO * cos l <: r_GEO * sin l <:. _0)

-- | ECEF @PosVel@ of a perfectly geostationary SC.
perfectGEO' :: RealFloat a => Longitude a -> PosVel ECR a
perfectGEO' l = C' (c $ perfectGEO l) (_0 <: _0 <:. _0)


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


-- Angle representations
-- ---------------------

-- | @timeAngle hours minutes seconds@ interprets the given time as an
  -- angle, where 24 h corresponds to 360°.
timeAngle :: Floating a => a -> a -> a -> Angle a
timeAngle h m s = h *~ hour + m *~ minute + s *~ second
  where
    hour   = mkUnitR (atom "" "" "") (1 Prelude./ 24) revolution
    minute = mkUnitR (atom "" "" "") (1 Prelude./ 60) hour
    second = mkUnitR (atom "" "" "") (1 Prelude./ 60) minute

-- | @sexagesimalAngle degrees arcminutes arcseconds@ returns the
  -- corresponding angle.
sexagesimalAngle :: Floating a => a -> a -> a -> Angle a
sexagesimalAngle d m s = d *~ degree + m *~ arcminute + s *~ arcsecond

-- | @dmsAngle@ is a synonym for `sexagesimalAngle`.
dmsAngle :: Floating a => a -> a -> a -> Angle a
dmsAngle = sexagesimalAngle
