{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Astro.Trajectory
  ( Trajectory, startTime, endTime, ephemeris
  , EphemTrajectory (ET)
  ) where

import Data.Maybe --(mapMaybe)
import Data.List (mapAccumL, unfoldr)
import Numeric.Units.Dimensional.Prelude
import Astro.Time
import Astro.Orbit.MEOE
import Astro.Orbit.Types
import Interpolate
import qualified Prelude

class Fractional a => Trajectory x t a
  where
    startTime :: x -> E t a
    endTime   :: x -> E t a
    ephemeris :: x -> E t a -> E t a -> Time a -> [Datum t a]



type Datum t a = (E t a, MEOE Mean a)

data EphemTrajectory t a = ET [Datum t a]


meoeAt :: (RealFloat a, Ord a) => [Datum t a] -> E t a -> MEOE Mean a
meoeAt []         _ = error "Can not interpolate an empty list"
meoeAt (x:[])     _ = snd x
meoeAt (x1:x2:[]) t = linearPolateMEOEm x1 x2 t
meoeAt (x1:x2:xs) t = if t < fst x2 then linearPolateMEOEm x1 x2 t
                                    else meoeAt (x2:xs) t

-- | Interpolate linearly in an ordered list of ephemeris to obtain
-- the a MEOE at the given time. If the given time is outside the
-- range covered by the list @Nothing@ is returned.
meoeAtMaybe :: (RealFloat a, Ord a) => [Datum t a] -> E t a -> Maybe (MEOE Mean a)
meoeAtMaybe []         _ = Nothing
meoeAtMaybe [(t',m)]   t = if t == t' then Just m else Nothing
meoeAtMaybe (x1:x2:xs) t = if t > fst x2 then meoeAtMaybe (x2:xs) t
                             else if t < fst x1 then Nothing
                               else Just (linearPolateMEOEm x1 x2 t)

-- | Interpolate linearly in an ordered list of ephemeris to obtain
-- the a MEOE at the given time. If the given time is outside the
-- range covered by the list @Nothing@ is returned.
meoeAtMaybe' :: (RealFloat a, Ord a) => [Datum t a] -> E t a -> ([Datum t a], Maybe (Datum t a))
meoeAtMaybe' []     _ = ([], Nothing)
meoeAtMaybe' (x:xs) t = case compare t (fst x) of
                          LT -> (x:xs, Nothing)
                          EQ -> (x:xs, Just x)
                          GT -> go x xs t
  where
    go _ [] _ = ([], Nothing)
    go x0 (x:xs) t = case compare t (fst x) of
                       LT -> (x0:x:xs, Just (t, linearPolateMEOEm x0 x t))
                       EQ -> (x:xs, Just x)
                       GT -> go x xs t


mobo :: (RealFloat a, Ord a) => [Datum t a] -> [E t a] -> [Datum t a]
mobo [] _ = []
mobo _ [] = []
mobo (x:xs) (t:ts) = case compare t (fst x) of
                       LT -> mobo xs (t:ts)
                       EQ -> x:mobo (x:xs) ts
                       GT -> go x xs (t:ts)
  where
    go _   []     _     = []
    go x0 (x:xs) (t:ts) = case compare t (fst x) of
        LT -> (t, linearPolateMEOEm x0 x t):mobo (x0:x:xs) ts
        EQ -> x:mobo (x:xs) ts
        GT -> go x xs (t:ts)

-- | Chronologically ordered arrays.
mobo22 :: (RealFloat a, Ord a) => [Datum t a] -> [E t a] -> [Datum t a]
mobo22 [] _ = []
mobo22 (x:xs) ts = mobo2 (x:xs) $ dropWhile (< fst x) ts


-- We already know that @t >= fst x@.
mobo2 :: (RealFloat a, Ord a) => [Datum t a] -> [E t a] -> [Datum t a]
mobo2 _    []   = []
mobo2 [x] (t:_) = if t == fst x then [x] else []
mobo2 (x0:x1:xs) (t:ts)
  | t == fst x0 = x0:mobo2 (x0:x1:xs) ts
  | t >= fst x1 = mobo2 (x1:xs) (t:ts)
  | otherwise   = (t, linearPolateMEOEm x0 x1 t):mobo (x0:x1:xs) ts


instance (RealFloat a) => Trajectory (EphemTrajectory t a) t a
  where
    startTime (ET []) = mjd' 0
    startTime (ET xs) = fst (head xs)
    endTime   (ET []) = mjd' 0
    endTime   (ET xs) = fst (last xs)
    -- ephemeris (ET xs) t0 t1 interval = mapMaybe (meoeAtMaybe xs) [t0, addTime t0 interval..t1]  -- SLOW!!!
    -- ephemeris (ET xs) t0 t1 interval = catMaybes $ snd $ mapAccumL meoeAtMaybe' xs ts
    --ephemeris (ET xs) t0 t1 interval = mobo xs ts
    ephemeris (ET xs) t0 t1 interval = mobo22 xs ts
      where
        ts = takeWhile (<=t1) $ iterate (`addTime` interval) t0
        --ts = unfoldr (\t -> if t > t1 then Nothing else Just (t, addTime t interval)) t0
        --ts = [t0, addTime t0 interval..t1]  -- Requires Enum (E t a)!
