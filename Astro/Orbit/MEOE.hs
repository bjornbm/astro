{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Astro.Orbit.MEOE where

import Numeric.NumType (incr)
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

vec2meoe v = MEOE (vElemAt zero v)
                  (vElemAt pos1 v)
                  (vElemAt pos2 v)
                  (vElemAt pos3 v)
                  (vElemAt pos4 v)
                  (vElemAt pos5 v)
                  (Long $ vElemAt (incr pos5) v)
