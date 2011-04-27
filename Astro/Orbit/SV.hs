module Astro.Orbit.SV where

import PosVel (CPos, CVel)

type SV a = (CPos a, CVel a)
