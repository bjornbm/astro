{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module TestUtil where

import Numeric.Units.Dimensional (Dimensional (Dimensional))
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.Vector (Vec (ListVec))
import qualified Prelude
import Astro.Time
import Astro.Coords
import Astro.Coords.PosVel
import Astro.Place
import Astro.Place.Topocentric
import Astro.Place.ReferenceEllipsoid
import Test.QuickCheck
import Control.Applicative
import Data.AEq



-- Instances (TODO: move elsewhere)
-- ================================

-- Approximate equality
-- --------------------

instance AEq a => AEq (Quantity d a)
  where
    (Dimensional x) === (Dimensional y) = x === y
    (Dimensional x) ~== (Dimensional y) = x ~== y

instance (Floating a, AEq a) => AEq (Vec ds a)  -- CPos et al
  where
    ListVec xs === ListVec ys = and $ zipWith (===) xs ys
    ListVec xs ~== ListVec ys = and $ zipWith (~==) xs ys

instance (Floating a, AEq a) => AEq (Coord s a)
  where
    r1 === r2 = c r1 === c r2
    r1 ~== r2 = c r1 ~== c r2

instance (RealFloat a, AEq a) => AEq (E t a)
  where
    E t1 === E t2 = t1 === t2
    E t1 ~== E t2 = t1 ~== t2

instance (RealFloat a, AEq a) => AEq (PosVel s a)
  where
    pv1 === pv2 = cpos pv1 === cpos pv2 && cvel pv1 === cvel pv2
    pv1 ~== pv2 = cpos pv1 ~== cpos pv2 && cvel pv1 ~== cvel pv2

-- Arbitrary instances
-- -------------------
instance (Arbitrary a) => Arbitrary (Quantity d a) where
  arbitrary   = Dimensional <$> arbitrary

--instance Arbitrary a => Arbitrary (CPos a) where
instance (VTuple (Vec ds a) t, Arbitrary t) => Arbitrary (Vec ds a)
  where
    arbitrary = fromTuple <$> arbitrary

instance Arbitrary a => Arbitrary (Coord s a)
  where
    --arbitrary = return . C . fromTuple =<< arbitrary
    arbitrary = C <$> arbitrary

instance (Fractional a, Arbitrary a) => Arbitrary (GeodeticPlace a)
  where
    arbitrary = GeodeticPlace <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Num a, Arbitrary a) => Arbitrary (ReferenceEllipsoid a)
  where
    arbitrary = do
      x <- arbitrary
      let pol = abs x + 1*~meter  -- Prevent zero radius.
      d <- arbitrary
      let eq  = pol + abs d       -- Always larger than polar radius.
      return (ReferenceEllipsoid eq pol)

instance (Arbitrary a, Fractional a) => Arbitrary (E t a) where
  arbitrary = mjd' <$> arbitrary

instance (Arbitrary a, Fractional a) => Arbitrary (PosVel s a) where
  arbitrary = C' <$> arbitrary <*> arbitrary


-- Helpers
-- =======

onceCheck :: (Testable prop) => prop -> IO ()
onceCheck = quickCheckWith stdArgs { maxSuccess = 1 }
manyCheck :: (Testable prop) => prop -> IO ()
manyCheck = quickCheckWith stdArgs { maxSuccess = 1000, maxDiscardRatio = 1 }

-- | Comparison allowing for specified inaccuracy.
cmpE :: (Fractional a, Ord a) => Quantity d a -> Quantity d a -> Quantity d a -> Bool
cmpE accuracy x y' = abs (x - y') < accuracy
