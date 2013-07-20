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
import Astro.Trajectory
import Data.List
import Data.Maybe (fromJust)


data Maneuver a = ImpulsiveRTN { dvr :: Velocity a
                               , dvt :: Velocity a
                               , dvn :: Velocity a
                               } deriving (Show, Eq)

data ManTrajectory t a = forall x. Trajectory x t a => MT (x t a) (Maybe (At t a (Maneuver a)))

applyManeuver :: Trajectory x t a => x t a -> At t a (Maneuver a) -> ManTrajectory t a
applyManeuver x = MT x . Just

applyManeuvers :: Trajectory x t a => x t a -> [At t a (Maneuver a)] -> ManTrajectory t a
applyManeuvers x []     = MT x Nothing
applyManeuvers x [m]    = applyManeuver x m  -- Redundant but avoids extra layer with Nothing.
applyManeuvers x (m:ms) = foldl' applyManeuver (applyManeuver x m) ms


instance (Fractional a, Ord a) => Trajectory ManTrajectory t a where
  startTime (MT x _) = startTime x
  endTime   (MT x _) = endTime   x
  ephemeris (MT x Nothing) = ephemeris x
  ephemeris (MT x (Just (man`At`tman))) = map massage . ephemeris x
    where
      --manRA = rightAscension $ c2s $ fst $ meoe2sv $ meoeM2meoe $ value $fromJust $ datum x tman
      massage (m`At`t) = (if t < tman then m else m') `At` t
        where m' = m  -- NOOP!!! TODO
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
