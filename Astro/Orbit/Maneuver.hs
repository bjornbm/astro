{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Astro.Orbit.Maneuver where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro.Time
import Astro.Orbit.Types
import Astro.Trajectory
import Data.List


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
      massage (m`At`t) = (if t < tman then m else m) `At` t -- NOOP!!! TODO!!!
        -- This is not generally valid as longitude acceleration is dependent
        -- on longitude. Will have to be more rigorous for trajectories where
        -- longitude varies significantly (TODO GEO).
        -- Perhaps applyManeuver needs to be member of Trajectory, or a
        -- subclass thereof?
