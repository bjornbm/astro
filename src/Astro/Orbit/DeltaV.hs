{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Astro.Orbit.DeltaV where

import Astro.Coords
import Astro.Coords.PosVel
import Astro.Coords.TNR
import Astro.Orbit.SV
import Astro.Orbit.Types
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.PosVel
import Numeric.Units.Dimensional.Prelude

newtype ImpulsiveDV frame a = DV (CVel a)

applyEciDV :: RealFloat a => ImpulsiveDV ECI a -> SV a -> SV a
applyEciDV (DV dv) sv = C' (cpos sv) (cvel sv `elemAdd` dv)

applyTnrDV :: RealFloat a => ImpulsiveDV TNR a -> SV a -> SV a
applyTnrDV (DV dv) sv = C' (cpos sv) v
  where
    v = cvel sv >+< (transpose (orbitalFrame sv) |*< dv)

-- | Convert to the deprecated `Maneuver` data type.
toManeuver :: ImpulsiveDV TNR a -> Maneuver a
toManeuver (DV dv) =
  let dvt = x dv
      dvn = y dv
      dvr = z dv
   in ImpulsiveRTN {..}

-- | Convert from the deprecated `Maneuver` data type.
fromManeuver :: Maneuver a -> ImpulsiveDV TNR a
fromManeuver ImpulsiveRTN {..} = DV (fromTuple (dvt, dvn, dvr))

-- impulsivePerturbation' :: RealFloat a => MEOE True a -> Maneuver a -> MEOE True a
-- impulsivePerturbation' meoe man = sv2meoe (mu meoe) m'
--   where
--     m' = applyTnrDV (fromManeuver man) (meoe2sv meoe)
