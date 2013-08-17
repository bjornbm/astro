module Astro.Orbit.SV where

import Numeric.Units.Dimensional.LinearAlgebra.PosVel (CPos, CVel)

-- | This is essentially identical to 'Astro.Coords.PosVel.PosVel'.
-- TODO: Why have both?
data SV system a = SV { pos :: CPos a
                      , vel :: CVel a
                      }  deriving (Eq, Show)


