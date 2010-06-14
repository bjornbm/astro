{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Astro.Time.ForwardAD where

import qualified Prelude
import Data.HList (HMap)
import Data.HList.HZip
import MyHList (HZipWith)
import Astro.Time (E (E), diffEpoch)
import Numeric.Units.Dimensional.Prelude
import VectorAD
import Vector
import Numeric.AD (Mode, AD)


-- | If @f@ is a function of epoch that returns a 'Vector', then
-- @diff f@ is a function of that returns the first derivative of
-- the result.
diffVt :: (Num a, HMap (DivD,DTime) ds ds')
        => (forall tag. E t (AD tag a) -> Vec ds (AD tag a))
        -> E t a -> Vec ds' a
diffVt f (E x) = diffV (f . E) x

-- | Like 'diffVt' but returns a pair of the result and its first derivative.
diffVt' :: (Num a, HMap (DivD,DTime) ds ds')
        => (forall tag. E t (AD tag a) -> Vec ds (AD tag a))
        -> E t a -> (Vec ds a, Vec ds' a)
diffVt' f (E x) = diffV' (f . E) x


applyLinearAtT :: forall a t ds ds' ds2 ds2' ts l'. (
                 Real a, Fractional a,
                 HMap (MulD,DTime) ds' ds,               -- Used in linearization.
                 HMap (DivD,DTime) ds2 ds2',             -- Used in differentiation.
                 HZipWith DivD ds ds' ts, Homo ts DTime, -- Necessary to infer t (the dimension w r t which we are differentiating).
                 HZip ds ds' l', HMap DivD l' ts         -- Needed to use applyLinearAt, not sure why, grr!
            ) => (forall tag. E t (AD tag a) -> Vec ds (AD tag a) -> Vec ds2 (AD tag a))
              -> E t a -> (Vec ds a, Vec ds' a) -> (Vec ds2 a, Vec ds2' a)
applyLinearAtT f (E t) = applyLinearAt (f . E) t
--applyLinearAtT f (p,v) t = diffVt' (\t' -> f (liftV p `elemAdd` scaleVec (diffEpoch t' (liftT t)) (liftV v)) t') t


-- Lift an epoch.
liftT :: (Mode tag, Num a) => E t a -> E t (AD tag a)
liftT (E t) = E (lift t)

