{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{- | Module for converting between ECR and other frames. This module is
-- currently somewhat unprincipled in that is uses constants for
-- Earth's rate of rotation rather than variable Earth rotation angle
  --
  -- TODO Use @Astro.Time.Sidereal.era@ instead of @Astrodynamics@.
  --
  -}
module Astro.Coords.ECR where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.Rotation
import Astro.Coords
import Astro.Coords.PosVel
import Astro.Time
import Astro.Time.At
import Astro.Time.Sidereal (gmst')
import qualified Astrodynamics


-- | Calculate the TNR orbital coordinate frame of the satellite with the
-- given ECR PosVel. The frame is defined by the Radial and Normal axes,
-- with the implication that the Tangential axis is not strictly aligned
-- with the velocity vector (i.e. “tangential” only for circular orbits).
orbitalFrameECR :: RealFloat a => PosVel ECR a -> CoordSys a
orbitalFrameECR pv = consRow t $ consRow n $ rowMatrix r where
  r = normalize $ cpos pv
  t = normalize $ crossProduct n r
  n = normalize $ crossProduct (cpos pv) v
  v = (w `crossProduct` cpos pv) >+< cvel pv
  w = 0*~day^neg1 <: 0*~day^neg1 <:. Astrodynamics.phi -- TODO copout using phi!

-- | Converts ECR position to orbital frame defined by PosVel.
ecrToOrbitalFrame :: RealFloat a => PosVel ECR a -> Coord ECR a -> Coord Orbital a
ecrToOrbitalFrame pv p = C $ orbitalFrameECR pv `matVec` (c p `elemSub` cpos pv)


-- | Convert coordinates from ECI frame to ECR frame.
eciToECR :: RealFloat a => E UT1 a -> Coord ECI a -> Coord ECR a
eciToECR t xyz = C (rotZ (negate $ gmst' t) |*< c xyz)
                                -- TODO ^^^ Use ERA instead??
-- | Convert coordinates from ECI frame to ECR frame.
eciToECR' :: RealFloat a => At UT1 a (Coord ECI a) -> At UT1 a (Coord ECR a)
eciToECR' At {..} = eciToECR epoch value `At` epoch

-- | Convert coordinates from ECR frame to ECI frame.
ecrToECI :: RealFloat a => E UT1 a -> Coord ECR a -> Coord ECI a
ecrToECI t xyz = C (rotZ (gmst' t) |*< c xyz)
                   -- TODO ^^^ Use gmst instead??
-- | Convert coordinates from ECR frame to ECI frame.
ecrToECI' :: RealFloat a => At UT1 a (Coord ECR a) -> At UT1 a (Coord ECI a)
ecrToECI' At {..} = ecrToECI epoch value `At` epoch


-- | Convert PosVel from ECR frame to ECI frame.
ecrToECIPV :: RealFloat a => E UT1 a -> PosVel ECR a -> PosVel ECI a
ecrToECIPV = liftPVAt ecrToECI

-- | Convert PosVel from ECR frame to ECI frame.
ecrToECIPV' :: RealFloat a => At UT1 a (PosVel ECR a) -> At UT1 a (PosVel ECI a)
ecrToECIPV' = liftPVAt' ecrToECI

-- | Convert PosVel from ECR frame to ECI frame.
eciToECRPV :: RealFloat a => E UT1 a -> PosVel ECI a -> PosVel ECR a
eciToECRPV = liftPVAt eciToECR

-- | Convert PosVel from ECR frame to ECI frame.
eciToECRPV' :: RealFloat a => At UT1 a (PosVel ECI a) -> At UT1 a (PosVel ECR a)
eciToECRPV' = liftPVAt' eciToECR
