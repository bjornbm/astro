{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module TestInstances where

import Control.Applicative
import Test.QuickCheck hiding (property)-- (Arbitrary, arbitrary, (==>))
import Data.AEq
import TestUtil
import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro.Util (plusMinusPi, zeroTwoPi)
import Astro.Orbit.Types
import Astro.Orbit.MEOE as M -- (MEOE (MEOE), meoe2vec)
import qualified Astro.Orbit.COE as C -- (COE (COE), coe2vec)
import Astro.Orbit.SV (SV)
import Astro.Orbit.Conversion (meoe2coe)
import Astro.Orbit.Maneuver


-- Instances
deriving instance Arbitrary a => Arbitrary (SemiMajorAxis a)
deriving instance Arbitrary a => Arbitrary (SemiLatusRectum a)
deriving instance Arbitrary a => Arbitrary (Anomaly t a)
deriving instance Arbitrary a => Arbitrary (Longitude t a)

-- Arbitrary instance always returns values >= 0.
instance (Num a, Ord a, Arbitrary a) => Arbitrary (Eccentricity a) where
    arbitrary = do
      NonNegative e <- arbitrary
      return $ Ecc (e*~one)

instance Arbitrary a => Arbitrary (Maneuver a) where
  arbitrary = ImpulsiveRTN <$> arbitrary <*> arbitrary <*> arbitrary

{-
instance (RealFloat a, Ord a, Arbitrary a) => Arbitrary (MEOE True a) where
  arbitrary = do
    Positive mu <- arbitrary
    sv2meoe (mu *~ (kilo meter ^ pos3 / second ^ pos2)) <$> arbitrary
-- -}

-- This instance will not generate orbits with very large eccentricities.
instance (RealFrac a, Ord a, Arbitrary a) => Arbitrary (M.MEOE t a) where
  arbitrary = do
    Positive mu <- arbitrary
    Positive p  <- arbitrary
    M.MEOE (mu *~ (kilo meter ^ pos3 / second ^ pos2)) (p *~ meter)
      <$> arbitrary1 <*> arbitrary1 <*> arbitrary1 <*> arbitrary1 <*> arbitrary
    where arbitrary1 = (*~one) . snd . properFraction <$> arbitrary

instance (RealFloat a, Ord a, Arbitrary a) => Arbitrary (C.COE t a) where
  arbitrary = meoe2coe <$> arbitrary

deriving instance AEq a => AEq (SemiMajorAxis a)
deriving instance AEq a => AEq (SemiLatusRectum a)
deriving instance AEq a => AEq (Eccentricity a)

instance (RealFloat a, Eq a) => Eq (Anomaly t a) where
  (Anom x) == (Anom y) = x ==~ y

instance (RealFloat a, AEq a) => AEq (Anomaly t a) where
  (Anom x) ~== (Anom y) = x ~==~ y

instance (RealFloat a, Eq a) => Eq (Longitude l a) where
  (Long x) == (Long y) = x ==~ y

instance (RealFloat a, AEq a) => AEq (Longitude l a) where
  (Long x) ~== (Long y) = x ~==~ y

deriving instance (RealFloat a,  Eq a) =>  Eq (M.MEOE l a)
deriving instance (RealFloat a,  Eq a) =>  Eq (C.COE t a)

instance (RealFloat a, AEq a) => AEq (M.MEOE t a) where
--m0 ~== m1 = meoe2vec m0 ~== meoe2vec m1
  m0 ~== m1 = M.mu m0 ~== M.mu m1
           && M.p  m0 ~== M.p  m1
           && M.f  m0 ~== M.f  m1
           && M.g  m0 ~== M.g  m1
           && M.h  m0 ~== M.h  m1
           && M.k  m0 ~== M.k  m1
           && long (M.longitude m0) ~==~ long (M.longitude m1)

instance (RealFloat a, AEq a) => AEq (C.COE t a) where
--c0 ~== c1 = C.coe2vec c0 ~== C.coe2vec c1
  c0 ~== c1 = C.mu   c0 ~== C.mu   c1
           && C.slr  c0 ~== C.slr  c1
           && C.ecc  c0 ~== C.ecc  c1
           && C.inc  c0 ~== C.inc  c1
           && C.aop  c0 ~== C.aop  c1
           && C.raan c0 ~== C.raan c1
           && anom (C.anomaly c0) ~==~ anom (C.anomaly c1)
