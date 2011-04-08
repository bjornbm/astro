module Astro.Coords.TNR where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import PosVel
import Astro.Coords
import Astro.Coords.PosVel

type CoordSys a = Homo33 DOne a

-- | Calculate the orbital coordinate frame of the satellite with the
-- given ECI PosVel.
orbitalFrame :: RealFloat a => PosVel ECI a -> CoordSys a
orbitalFrame pv = consRow t $ consRow n $ rowMatrix r where
  r = vNormalize (cpos pv)
  n = vNormalize (crossProduct (cpos pv) (cvel pv))
  t = vNormalize $ crossProduct n r


-- | Converts ECI position to orbital frame defined by PosVel.
-- TODO Defining PosVel should be in ECI. Enforce this! (Coord needs to
-- support velocities and PosVels to methinks.)
eciToOrbitalFrame :: RealFloat a => PosVel ECI a -> Coord ECI a -> Coord Orbital a
eciToOrbitalFrame pv p = C $ orbitalFrame pv `matVec` (c p `elemSub` cpos pv)
