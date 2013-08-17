module Astro.Orbit.SV where

import Numeric.Units.Dimensional.LinearAlgebra.PosVel (CPos, CVel)

data SV system a = SV (CPos a) (CVel a) deriving (Eq, Show)
