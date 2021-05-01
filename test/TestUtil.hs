{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module TestUtil where

import Control.Applicative
import Data.AEq
import qualified Debug.Trace
import Test.QuickCheck

import Numeric.Units.Dimensional.Prelude
-- import Numeric.Units.Dimensional.DK.AEq
import qualified Prelude

import Astro.Util.Cyclic


-- | Convenience type for QuickCheck.
type D = Double


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
