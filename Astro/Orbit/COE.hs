{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Astro.Orbit.COE where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Astro.Orbit.Types

-- | Classical orbital elements.
data COE a = COE
  { mu   :: GravitationalParameter a
  , slr  :: Length a  -- Semi-latus rectum.
  , ecc  :: Dimensionless a
  , inc  :: Angle a
  , aop  :: Angle a
  , raan :: Angle a
  , trueAnomaly :: TrueAnomaly a
  } deriving (Show)

-- | Convert a COE into a vector (not a State Vector) of its
-- elements (including mu).
coe2vec :: COE a -> Vec (DGravitationalParameter:*:DLength:*:DOne:*:DPlaneAngle:*:DPlaneAngle:*:DPlaneAngle:*.DPlaneAngle) a
coe2vec COE{..} = mu <: slr <: ecc <: inc <: aop <: raan <:. ta trueAnomaly
