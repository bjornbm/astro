module Astro.Coords.TNR where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.Operators
import Numeric.Units.Dimensional.LinearAlgebra.PosVel
import Astro.Coords
import Astro.Coords.PosVel
import qualified Astrodynamics


-- | Calculate the TNR orbital coordinate frame of the satellite with the
-- given ECI PosVel. The frame is defined by the Radial and Normal axes,
-- with the implication that the Tangential axis is not strictly aligned
-- with the velocity vector (i.e. “tangential” only for circular orbits).
orbitalFrame :: RealFloat a => PosVel ECI a -> CoordSys a
orbitalFrame pv = consRow t $ consRow n $ rowMatrix r where
  r = vNormalize (cpos pv)
  n = vNormalize (crossProduct (cpos pv) (cvel pv))
  t = vNormalize $ crossProduct n r


-- | Converts ECI position to orbital frame defined by PosVel.
eciToOrbitalFrame :: RealFloat a => PosVel ECI a -> Coord ECI a -> Coord Orbital a
eciToOrbitalFrame pv p = C $ orbitalFrame pv `matVec` (c p `elemSub` cpos pv)


-- Move the below to an ECR module??

-- | Calculate the TNR orbital coordinate frame of the satellite with the
-- given ECR PosVel. The frame is defined by the Radial and Normal axes,
-- with the implication that the Tangential axis is not strictly aligned
-- with the velocity vector (i.e. “tangential” only for circular orbits).
orbitalFrameECR :: RealFloat a => PosVel ECR a -> CoordSys a
orbitalFrameECR pv = consRow t $ consRow n $ rowMatrix r where
  r = vNormalize (cpos pv)
  t = vNormalize $ crossProduct n r
  n = vNormalize (crossProduct (cpos pv) v)
  v = (w `crossProduct` cpos pv) >+< cvel pv
  w = 0*~day^neg1 <: 0*~day^neg1 <:. Astrodynamics.phi -- TODO copout using phi!

-- | Converts ECR position to orbital frame defined by PosVel.
ecrToOrbitalFrame :: RealFloat a => PosVel ECR a -> Coord ECR a -> Coord Orbital a
ecrToOrbitalFrame pv p = C $ orbitalFrameECR pv `matVec` (c p `elemSub` cpos pv)

