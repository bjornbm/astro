module Astro.Coords.TNR where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Vector
import Matrix
import PosVel
import Astro.Coords

type CoordSys a = Homo33 DOne a

-- | Calculate the orbital coordinate frame of the satellite with the
-- given ECI PosVel.
orbitalFrame :: RealFloat a => CPosVel a -> CoordSys a
orbitalFrame (pos, vel) = consRow t $ consRow n $ rowMatrix r where
  r = vNormalize pos
  n = vNormalize (crossProduct pos vel)
  t = vNormalize $ crossProduct r n


-- | Converts ECI position to orbital frame defined by PosVel.
-- TODO Defining PosVel should be in ECI. Enforce this! (Coord needs to
-- support velocities and PosVels to methinks.)
eciToOrbitalFrame :: RealFloat a => CPosVel a -> Coord ECI a -> Coord Orbital a
eciToOrbitalFrame pv p = C $ orbitalFrame pv `matVec` (c p `elemSub` fst pv)



