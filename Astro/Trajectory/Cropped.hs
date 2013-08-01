{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Astro.Trajectory.Cropped ( cropStart,  cropEnd,  crop
                                , cropStart', cropEnd', crop'
                                ) where

import Data.Maybe (fromMaybe)
import Astro.Time
import Astro.Trajectory

-- Wrapper data type used to crop the validity (in time) of an
-- arbitrary @Trajectory@.
data CroppedTraj x t a = CS (E t a) (x t a)
                       | CE (E t a) (x t a)
                       | C0 (x t a)
                       | C2 (CroppedTraj (CroppedTraj x) t a)

instance (Trajectory x t a) => Trajectory (CroppedTraj x) t a where
  startTime (CS t0 traj) = max t0 (startTime traj)
  startTime (CE _  traj) = startTime traj
  startTime (C0    traj) = startTime traj
  startTime (C2    traj) = startTime traj
  endTime   (CS _  traj) = endTime traj
  endTime   (CE t1 traj) = min t1 (endTime traj)
  endTime   (C0    traj) = endTime traj
  endTime   (C2    traj) = endTime traj
  ephemeris (C0 traj) = ephemeris traj
  ephemeris (C2 traj) = ephemeris traj
  ephemeris ct@(CS _ traj) = ephemeris traj . dropWhile (< startTime ct)
  ephemeris ct@(CE _ traj) = ephemeris traj . takeWhile (<= endTime ct)

  -- We don't use default definition in case underlying trajectory has
  -- optimized @ephemeris'@.
  ephemeris' (CS t traj) t0 t1 dt = ephemeris' traj t0' t1 dt
    where t0' = head $ dropWhile (< t) $ iterate (`addTime` dt) t0
  ephemeris' (CE t traj) t0 t1 dt = ephemeris' traj t0 t1' dt
    where t1' = min t1 t
  ephemeris' (C0   traj) t0 t1 dt = ephemeris' traj t0 t1 dt
  ephemeris' (C2   traj) t0 t1 dt = ephemeris' traj t0 t1 dt

  -- We don't use default definition in case underlying trajectory has
  -- optimized @datum@.
  datum (CS t0 traj) t = if t >= t0 then datum traj t else Nothing
  datum (CE t1 traj) t = if t <= t1 then datum traj t else Nothing
  datum (C0    traj) t = datum traj t
  datum (C2    traj) t = datum traj t


-- | @cropStart t0 traj@ crops @traj@ so it starts no earlier than @t0@.
-- If @startTime traj >= t0@ the @Trajectory@ is effectively unchanged.
cropStart :: Trajectory x t a => E t a -> x t a -> CroppedTraj x t a
cropStart t0 x = CS t0 x

-- | @cropEnd t1 traj@ crops @traj@ so it end no later than @t1@.
-- If @endTime traj <= t1@ the @Trajectory@ is effectively unchanged.
cropEnd :: Trajectory x t a => E t a -> x t a -> CroppedTraj x t a
cropEnd t1 x = CE t1 x

crop :: Trajectory x t a => E t a -> E t a -> x t a -> CroppedTraj x t a
crop t0 t1 = C2 . cropStart t0 . cropEnd t1

cropStart' :: Trajectory x t a => Maybe (E t a) -> x t a -> CroppedTraj x t a
cropStart' Nothing = C0
cropStart' (Just t0) = cropStart t0
cropEnd'   :: Trajectory x t a => Maybe (E t a) -> x t a -> CroppedTraj x t a
cropEnd'   Nothing = C0
cropEnd'   (Just t1) = cropEnd t1
crop' :: Trajectory x t a => Maybe (E t a) -> Maybe (E t a) -> x t a -> CroppedTraj x t a
crop' Nothing   Nothing   = C0
crop' (Just t0) Nothing   = cropStart t0
crop' Nothing   (Just t1) = cropEnd t1
crop' (Just t0) (Just t1) = C2 . cropStart t0 . cropEnd t1
