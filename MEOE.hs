{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module MEOE where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude
import PosVel (CPos, CVel)

import COE

type SV a = (CPos a, CVel a)

-- | Modified Equinoctial Orbital Elements as defined by Walker et al.
data MEOE a = MEOE
  { mu :: GravitationalParameter a
  , p  :: Length a
  , f  :: Dimensionless a
  , g  :: Dimensionless a
  , h  :: Dimensionless a
  , k  :: Dimensionless a
  , l  :: Angle a
  } deriving (Show)

meoe2vec MEOE{..} = mu <: p <: f <: g <: h <: k <:. l


-- | Convert a COE into a MEOE. Note that in general the true
-- longitude of the resultwill not be in the range [-pi,pi).
coe2meoe :: RealFloat a => COE a -> MEOE a
coe2meoe COE {..} = MEOE
  { mu = mu
  , p  = slr
  , f  = ecc * cos (aop + raan)
  , g  = ecc * sin (aop + raan)
  , h  = tan (inc / _2) * cos raan
  , k  = tan (inc / _2) * sin raan
  , l  = raan + aop + trueAnomaly  -- May be beyond [-pi,pi).
  }

-- | Convert a MEOE into a COE. Use the algoritm from Eagle
-- except for trueAnomaly for which it has a singularity.
-- Note that in general the true anomaly of the result will
-- not be in the range [-pi,pi).
meoe2coe :: RealFloat a => MEOE a -> COE a
meoe2coe MEOE{..} = COE
  { mu   = mu
  , slr  = p
  , ecc  = sqrt (f ^ pos2 + g ^ pos2)
  , inc  = atan2 (_2 * sqrt (h ^ pos2 + k ^ pos2)) (_1 - h ^ pos2 - k ^ pos2)
  , aop  = aop
  , raan = raan
  , trueAnomaly = l - aop - raan  -- May be beyond [-pi,pi).
  --  trueAnomaly = l - atan (g / f)  -- Singular for f = 0!
  } where
      raan = atan2 k h
      aop  = atan2 (g * h - f * k) (f * h + g * k)

-- | Convert a MEOE to a cartesian State Vector.
meoe2sv :: RealFloat a => MEOE a -> SV a
meoe2sv MEOE{..} = ( (r_x <: r_y <:. r_z) >* (r / s2)
                   , (v_x <: v_y <:. v_z) >* negate (sqrt (mu / p) / s2)
                   )
  where
    -- Position
    r_x = cos l + a2 * cos l + _2 * h * k * sin l
    r_y = sin l - a2 * sin l + _2 * h * k * cos l
    r_z = _2 * (h * sin l - k * cos l)
    -- Velocity
    v_x =         sin l  + a2 * sin l - hk2 * cos l + g - f * hk2 + a2 * g
    v_y = negate (cos l) + a2 * cos l + hk2 * sin l - f + g * hk2 + a2 * f
    v_z = negate _2 * (h * cos l + k * sin l + f * h + g * k)
    -- where
    a2 = h ^ pos2 - k ^ pos2  -- alpha^2
    s2 = _1 + h ^ pos2 + k ^ pos2
    w  = _1 + f * cos l + g * sin l
    r  = p / w
    hk2 = _2 * h * k
