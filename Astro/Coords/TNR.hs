module Astro.Coords.TNR where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.Rotation (rotX)
import Astro.Coords
import Astro.Coords.PosVel
import qualified Astrodynamics


-- | Calculate the TNR orbital coordinate frame of the satellite with the
-- given ECI PosVel. The frame is defined by the Radial and Normal axes,
-- with the implication that the Tangential axis is not strictly aligned
-- with the velocity vector (i.e. “tangential” only for circular orbits).
orbitalFrame :: RealFloat a => PosVel ECI a -> CoordSys a
orbitalFrame pv = consRow t $ consRow n $ rowMatrix r where
  r = vNormalize $ cpos pv
  n = vNormalize $ crossProduct (cpos pv) (cvel pv)
  t = vNormalize $ crossProduct n r


-- | Converts ECI position to orbital frame defined by PosVel.
eciToOrbitalFrame :: RealFloat a => PosVel ECI a -> Coord ECI a -> Coord Orbital a
eciToOrbitalFrame pv p = C $ orbitalFrame pv `matVec` (c p `elemSub` cpos pv)

-- | Computes the attitude coordinate system (RPY) for a satellite where
-- the Z (yaw) axis is nadir-pointing, the Y (pitch) axis is normal to
-- the orbit plane, and the X (roll) axis completes the coordinate system.
-- (The X (roll) axis does not align with the velocity vector.)
attitudeCoordSys :: RealFloat a => PosVel ECI a -> CoordSys a
attitudeCoordSys pv = rotX pi |*| orbitalFrame pv
