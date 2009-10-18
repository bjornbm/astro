{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Astro.Time.ForwardAD where

import qualified Prelude
import Data.HList (HMap)
import Astro.Time (E (E))
import Numeric.Units.Dimensional.Prelude
import ForwardAD
import Vector
import Numeric.FAD (Dual)


-- | If @f@ is a function of epoch that returns a 'Vector', then
-- @diff f@ is a function of that returns the first derivative of
-- the result.
diffVt :: (Num a, HMap (DivD,DTime) ds ds')
        => (forall tag. E t (Dual tag a) -> Vec ds (Dual tag a))
        -> E t a -> Vec ds' a
diffVt f (E x) = diffV (f . E) x

-- | Like 'diffVt' but returns a pair of the result and its first derivative.
diffVt' :: (Num a, HMap (DivD,DTime) ds ds')
        => (forall tag. E t (Dual tag a) -> Vec ds (Dual tag a))
        -> E t a -> (Vec ds a, Vec ds' a)
diffVt' f (E x) = diffV' (f . E) x

-- Lift an epoch.
liftT :: Num a => E t a -> E t (Dual tag a)
liftT (E t) = E (lift t)

