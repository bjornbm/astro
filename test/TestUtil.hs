{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module TestUtil where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.AEq
import qualified Prelude
import Astro.Util
import Test.QuickCheck
import Control.Applicative
import Data.AEq
import qualified Debug.Trace



-- Helpers
-- =======

onceCheck :: (Testable prop) => prop -> IO ()
onceCheck = quickCheckWith stdArgs { maxSuccess = 1 }
manyCheck :: (Testable prop) => prop -> IO ()
manyCheck = quickCheckWith stdArgs { maxSuccess = 1000, maxDiscardRatio = 1 }

-- | Comparison allowing for specified inaccuracy.
cmpE :: (Fractional a, Ord a) => Quantity d a -> Quantity d a -> Quantity d a -> Bool
cmpE accuracy x y' = abs (x - y') < accuracy


-- Debugging
-- =========

-- | Trace the argument with a descriptive prefix.
trace :: Show a => String -> a -> a
trace s x = Debug.Trace.trace (s ++ ": " ++ show x) x


-- Massagers
-- =========

-- | Removes the integral part of a value so that it ends up in the
-- interval [0,1). This differs from (snd . properFraction) for
-- negative values:
--   @(snd . properFraction) (-1.2)@ = -0.2
--   @fractionalPart         (-1.2)@ =  0.8
fractionalPart :: Dimensionless Double -> Dimensionless Double
fractionalPart = fmap (\x -> x Prelude.- fromIntegral (floor x))


-- Angle comparisons
-- =================

-- | Compares two angles for cyclic equality.
(==~) :: (RealFloat a, Eq a) => Angle a -> Angle a -> Bool
x ==~ y = plusMinusPi x == plusMinusPi y

-- | Compares two angles for approximate cyclic equality.
(~==~) :: (RealFloat a, AEq a) => Angle a -> Angle a -> Bool
x ~==~ y = plusMinusPi x ~== plusMinusPi y
        ||   zeroTwoPi x ~==   zeroTwoPi y  -- move the boundaries.

infixl 4 ==~, ~==~
