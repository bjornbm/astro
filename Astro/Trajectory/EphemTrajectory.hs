{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A @Trajectory@ instance based on a chronologically ordered
-- list of data. The start and end times are determined by the
-- first and last datum in the list.
module Astro.Trajectory.EphemTrajectory
  ( EphemTrajectory (ET)
  ) where

import Numeric.Units.Dimensional.Prelude
import Astro.Orbit.MEOE
import Astro.Orbit.Types
import Astro.Trajectory
import Astro.Time
import Interpolate
import qualified Prelude


-- | Interpolate linearly in an ordered list of ephemeris to obtain
-- the MEOE at the given time. If the given time is outside the
-- range covered by the list the end element are extrapolated.
meoeAt :: (RealFloat a, Ord a) => [Datum t a] -> E t a -> MEOE Mean a
meoeAt []         _ = error "Can not interpolate an empty list"
meoeAt (x:[])     _ = snd x
meoeAt (x1:x2:[]) t = linearPolateMEOEm x1 x2 t
meoeAt (x1:x2:xs) t = if t < fst x2 then linearPolateMEOEm x1 x2 t
                                    else meoeAt (x2:xs) t

-- | Interpolate linearly in an ordered list of ephemeris to obtain
-- the MEOE at the given time. If the given time is outside the
-- range covered by the list @Nothing@ is returned.
meoeAtMaybe :: (RealFloat a, Ord a) => [Datum t a] -> E t a -> Maybe (MEOE Mean a)
meoeAtMaybe []         _ = Nothing
meoeAtMaybe [(t',m)]   t = if t == t' then Just m else Nothing
meoeAtMaybe (x1:x2:xs) t = if t > fst x2 then meoeAtMaybe (x2:xs) t
                             else if t < fst x1 then Nothing
                               else Just (linearPolateMEOEm x1 x2 t)


-- The input arrays must be chronologically ordered.
meoes :: (RealFloat a, Ord a) => [Datum t a] -> [E t a] -> [Datum t a]
meoes [] _ = []
meoes (x:xs) ts = go (x:xs) $ dropWhile (< fst x) ts
  where
    -- We already know that @xs@ is non-empty and @t >= fst x@.
    go :: (RealFloat a, Ord a) => [Datum t a] -> [E t a] -> [Datum t a]
    go _    []   = []
    go [x] (t:_) = if t == fst x then [x] else []
    go (x0:x1:xs) (t:ts)
      | t == fst x0 = x0:go (x0:x1:xs) ts
      | t >= fst x1 = go (x1:xs) (t:ts)
      | otherwise   = (t, linearPolateMEOEm x0 x1 t):go (x0:x1:xs) ts



newtype EphemTrajectory t a = ET [Datum t a]
  -- Should I skip this newtype and just define an instance for the list??

instance (RealFloat a) => Trajectory (EphemTrajectory t a) t a
  where
    startTime (ET []) = mjd' 0
    startTime (ET xs) = fst (head xs)
    endTime   (ET []) = mjd' 0
    endTime   (ET xs) = fst (last xs)
    ephemeris (ET xs) ts = meoes xs ts
