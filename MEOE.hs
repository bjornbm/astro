{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module MEOE where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude
import PosVel (CPos, CVel)

import COE

data MEOE a = MEOE
  { mu :: GravitationalParameter a
  , p  :: Length a
  , f  :: Dimensionless a
  , g  :: Dimensionless a
  , h  :: Dimensionless a
  , k  :: Dimensionless a
  , l  :: Angle a
  }

meoe2vec MEOE{..} = mu <: p <: f <: g <: h <: k <:. l

coe2meoe :: RealFloat a => COEt a -> MEOE a
coe2meoe COEt {..} = MEOE
  { mu = mu
  , p  = sma * (_1 - ecc ^ pos2)
  , f  = ecc * cos (aop + raan)
  , g  = ecc * sin (aop + raan)
  , h  = tan (inc / _2) * cos raan
  , k  = tan (inc / _2) * sin raan
  , l  = raan + aop + trueAnomaly
  }

meoe2coe :: RealFloat a => MEOE a -> COEt a
meoe2coe MEOE {..} = COEt
  { mu  = mu
  , sma = p / (_1 - f ^ pos2 - g ^ pos2)
  , ecc = sqrt (f ^ pos2 + g ^ pos2)
  , inc = atan2 (_2 * sqrt (h ^ pos2 + k ^ pos2)) (_1 - h ^ pos2 - k ^ pos2)
  , aop = atan2 (g * h - f * k) (f * h + g * k)
  , raan = atan2 k h
  , trueAnomaly = l - atan (g / f)
  }



sv2meoe :: RealFloat a => GravitationalParameter a -> CPos a -> CVel a -> MEOE a
sv2meoe mu r' v' = MEOE
  { mu = mu
  , p  = h ^ pos2 / mu
  , f  = undefined -- ecc * cos (aop + raan)
  , g  = undefined -- ecc * sin (aop + raan)
  , h  = undefined -- tan (inc / _2) * cos raan
  , k  = undefined -- tan (inc / _2) * sin raan
  , l  = undefined -- raan + aop + trueAnomaly
  }
  where
    r  = vNorm r'
    (x,y,z) = toTuple r'
    v  = vNorm v'
    h' = r' `crossProduct` v'
    h  = vNorm h'
    (h_x, h_y, h_z) = toTuple h'
    raan = atan2 (h_x) (negate h_y)
    --trueAnomaly = atan2 (h * r * v) (h ^ pos2 - mu * r)
    u = acos ((x * cos raan + y * sin raan) / r)
    e' = v' `crossProduct` h' >/ mu >-< vNormalize r'
    e  = vNorm e'
    trueAnomaly = sideOfPeri * acos (r' >.< e' / (r * e))
    sideOfPeri = negate $ r' >.< v' / (r * v)


r::CPos Double;r=undefined
v::CVel Double;v=undefined
