{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Astro.Orbit.Maneuver where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro.Time
import Astro.Time.At
import Astro.Orbit.Types
import Astro.Orbit.Conversion
import Astro.Orbit.DeltaV
import Astro.Orbit.MEOE
import Astro.Trajectory
import Data.AEq
import Data.List
import Data.Maybe (fromJust)


-- | Apply a @Maneuver@ to an @MEOE@.
impulsivePerturbation :: RealFloat a => MEOE True a -> Maneuver a -> MEOE True a
impulsivePerturbation meoe man 
  | man == ImpulsiveRTN _0 _0 _0 = meoe  -- Ugly, but avoids MEOE->SV->MEOE conversion numerics.
  | otherwise = sv2meoe (mu meoe) m'
  where
    m' = applyTnrDV (fromManeuver man) (meoe2sv meoe)


data ManTrajectory t a = forall x. Trajectory x t a => MT (x t a) (Maybe (At t a (Maneuver a)))

applyManeuver :: Trajectory x t a => x t a -> At t a (Maneuver a) -> ManTrajectory t a
applyManeuver x = MT x . Just

applyManeuvers :: (RealFloat a, AEq a, Trajectory x t a)
               => x t a -> [At t a (Maneuver a)] -> ManTrajectory t a
applyManeuvers x []     = MT x Nothing
applyManeuvers x [m]    = applyManeuver x m  -- Redundant but avoids extra layer with Nothing.
applyManeuvers x (m:ms) = foldl' applyManeuver (applyManeuver x m) ms


instance (RealFloat a, Ord a, AEq a) => Trajectory ManTrajectory t a where
  startTime (MT x _) = startTime x
  endTime   (MT x _) = endTime   x
  ephemeris (MT x  Nothing            ) ts = ephemeris x ts
  ephemeris (MT x (Just (man`At`tman))) ts
    | tman < startTime x = ephemeris x ts  -- Ignore maneuver before validity
    | tman >   endTime x = ephemeris x ts  -- Ignore maneuver after validity
    | otherwise          = map massage $ ephemeris x ts
    where
      -- | Pre-maneuver MEOE at maneuver epoch
      m0  = value . fromJust $ datum x tman
      -- | Post-maneuver MEOE at maneuver epoch
      m0' = meoe2meoeM . flip impulsivePerturbation man . meoeM2meoe $ m0
      -- | Apply maneuver to datum, if datum is after maneuver.
      massage (m`At`t) = (if t < tman then m else m') `At` t
        where
          dt  = diffEpoch t tman
          dm1 = deltaMEOE (propagateUnperturbedFor m0' dt)
                          (propagateUnperturbedFor m0  dt)
          m' = applyMEOEDelta m dm1
        -- This is not generally valid as longitude acceleration is dependent
        -- on longitude. Will have to be more rigorous for trajectories where
        -- longitude varies significantly (TODO GEO).
        -- Perhaps applyManeuver needs to be member of Trajectory, or a
        -- subclass thereof?

maneuverToRelElems :: Floating a => At t a (Maneuver a) -> Datum t a -> Datum t a
maneuverToRelElems (man`At`tman) (m`At`t) = undefined {- m
  {

  }
  Elements t dl dd (e_x, e_y) (di * sin ra, di * negate (cos ra))

  where
    manRA =

    dl   = _2 * (e_y * cos ra - e_x * sin ra)
    dd   = negate (_3 * dv_t / r_GEO)
    de_t = _2 * dv_t / v_GEO
    de_r = dv_r / v_GEO
    e_x  = de_t * cos ra + de_r * sin ra
    e_y  = de_t * sin ra - de_r * cos ra
    di   = dv_n / v_GEO
-- -}
