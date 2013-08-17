module Astro.Time.At where

import Control.Applicative
import Data.Foldable
import Data.Traversable

import Astro.Time (E)

-- | Data type tagging some value x with a specific time. Typically
-- use is @x `At` t@.
-- Isomorphic to @(E t a, x)@ (also true for the 'Functor' instance).
data At t a x = At { value :: x
                   , epoch :: E t a
                   } deriving (Show, Eq)

instance Functor (At t a) where fmap f (x `At` t) = f x `At` t

instance Foldable (At t a) where
  foldMap f (x `At` t) = f x

instance Traversable (At t a) where
  traverse f (x `At` t) = (`At` t) <$> f x

-- | A flipped 'At', in other words @t `tA` x == x `At` t@.
tA :: E t a -> x -> At t a x
tA = flip At

-- | Convert the tuple (t, x) into @x `At` t@.
asAt :: (E t a, x) -> At t a x
asAt (t, x) = x `At` t
-- | Convert @x `At` t@ into the tuple (t, x).
unAt :: At t a x -> (E t a, x)
unAt (x `At` t) = (t, x)

-- | Kind of like an epoch dependent 'fmap'.
appAt :: (At t a x -> y) -> At t a x -> At t a y
appAt f at = at { value = f at }
-- | Maps 'appAt f'.
mapAt :: (At t a x -> y) -> [At t a x] -> [At t a y]
mapAt f = map (appAt f)
