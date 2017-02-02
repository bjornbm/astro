{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Astro.Time.ForwardAD where

import qualified Prelude
import Astro.Time (E (E), diffEpoch)
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.AD
import Numeric.Units.Dimensional.LinearAlgebra.VectorAD


-- | If @f@ is a function of epoch that returns a 'Vector', then
-- @diff f@ is a function of that returns the first derivative of
-- the result.
diffVt :: (Num a, d' ~ (/) d DTime)
        => (forall tag b . b ~ FAD tag a => E t b -> Vec d n b)
        -> E t a -> Vec d' n a
diffVt f (E x) = diffV (f . E) x

-- | Like 'diffVt' but returns a pair of the result and its first derivative.
diffVt' :: (Num a, d' ~ (/) d DTime)
        => (forall tag b . b ~ FAD tag a => E t b -> Vec d n b)
        -> E t a -> (Vec d n a, Vec d' n a)
diffVt' f (E x) = diffV' (f . E) x


applyLinearAtT :: (d1 ~ (*) DTime d1', d2' ~ (/) d2 DTime, Num a)
               => (forall tag b . b ~ FAD tag a => E t b -> Vec d1 n b -> Vec d2 m b)
               -> E t a -> (Vec d1 n a, Vec d1' n a) -> (Vec d2 m a, Vec d2' m a)
applyLinearAtT f (E t) = applyLinearAt (f . E) t


-- Lift an epoch.
instance Lift (E t) where lift (E t) = E (lift t)
