module Astro.Orbit.SV where

import Numeric.Units.Dimensional.LinearAlgebra.PosVel (CPos, CVel)

type SV a = (CPos a, CVel a)
