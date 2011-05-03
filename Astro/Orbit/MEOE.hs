{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Astro.Orbit.MEOE where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Astro.Orbit.Types
import qualified Prelude

-- | Modified Equinoctial Orbital Elements as defined by Walker et al.
data MEOE long a = MEOE
  { mu :: GravitationalParameter a
  , p  :: Length a
  , f  :: Dimensionless a
  , g  :: Dimensionless a
  , h  :: Dimensionless a
  , k  :: Dimensionless a
  , longitude :: Longitude long a
  } deriving (Show)

-- | Convert a MEOE into a vector (not a State Vector) of its
-- elements (including mu).
meoe2vec MEOE{..} = mu <: p <: f <: g <: h <: k <:. long longitude
