{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fcontext-stack=35 #-}

module COE where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude
import PosVel (CPos, CVel)


data COE a = COE
  { mu   :: GravitationalParameter a
  , slr  :: Length a  -- Semi-latus rectum.
  , ecc  :: Dimensionless a
  , inc  :: Angle a
  , aop  :: Angle a
  , raan :: Angle a
  , trueAnomaly :: Angle a
  } deriving (Show)

-- | Convert a COE into a vector (not a State Vector) of its
-- elements (including mu).
coe2vec :: COE a -> Vec (DGravitationalParameter:*:DLength:*:DOne:*:DPlaneAngle:*:DPlaneAngle:*:DPlaneAngle:*.DPlaneAngle) a
coe2vec COE{..} = mu <: slr <: ecc <: inc <: aop <: raan <:. trueAnomaly


-- | Convert state vector into Classical Orbital Elements. The
-- algorithm employed provides sound results for orbits with
-- inclination and/or eccentricity of zero.
--   i = 0   ->  RAAN = pi
--   i = pi  ->  RAAN = pi
--   e = 0   ->  AoP  = 0
sv2coe :: RealFloat a => GravitationalParameter a -> CPos a -> CVel a -> COE a
sv2coe mu r' v' = COE
  { mu   = mu
  , slr  = p
  , ecc  = vNorm e'
  , inc  = inc
  , aop  = aop
  , raan = raan
  , trueAnomaly = trueAnomaly
  }
  where
    -- Angular momentum.
    h' = r' `crossProduct` v'  -- Angular momentum vector.
    h  = vNorm h'
    (h_x, h_y, h_z) = toTuple h'

    -- Semi-latus rectum
    p = h ^ pos2 / mu

    -- Inclination and ascending node.
    inc  = acos (h_z / h)
    raan = atan2 (h_x) (negate h_y)   -- pi for inc == 0.
    n' = cos raan <: sin raan <:. _0  -- Unit vector pointing towards AN.
    m' = h' `crossProduct` n'  -- Vector 90° ahead of n' (AN) in orbit plane.

    -- Eccentricity and perigee.
    e' = v' `crossProduct` h' >/ mu >-< vNormalize r'  -- See e.g. Wikipedia.
    aop = atan2 (e' >.< vNormalize m') (e' >.< n')     -- 0 for e' == 0'.

    -- Argument of latitude (angle from AN to r').
    u = atan2 (r' >.< vNormalize m') (r' >.< n')
    -- True anomaly.
    trueAnomaly = u - aop



coe2sv COE{..} = undefined
  where
    r = slr / (_1 + ecc * cos trueAnomaly)
    x_orb = r * cos trueAnomaly
    y_orb = r * sin trueAnomaly
    z_orb = 0 *~ meter

