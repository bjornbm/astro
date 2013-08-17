module Astro.Orbit.SV where

import Numeric.Units.Dimensional.LinearAlgebra.PosVel (CPos, CVel)
import Astro.Coords
import Astro.Coords.PosVel

-- | This is essentially identical to 'Astro.Coords.PosVel.PosVel'.
-- TODO: Why have both?
type SV = PosVel ECI

sv :: CPos a -> CVel a -> SV a
sv = C'
pos :: Floating a => SV a -> CPos a
pos = cpos
vel :: RealFloat a => SV a -> CVel a
vel = cvel
coord :: Floating a => SV a -> Coord ECI a
coord = C . cpos
