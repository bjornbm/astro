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
import Astro.Orbit.MEOE (MEOE (MEOE))
import Astro.Orbit.SV (SV)
import Astro.Orbit.Conversion (sv2meoe)
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
    uncurry (sv2meoe (mu *~ (kilo meter ^ pos3 / second ^ pos2))) <$> arbitrary
-- -}

instance (RealFrac a, Ord a, Arbitrary a) => Arbitrary (MEOE True a) where
  arbitrary = do
    Positive mu <- arbitrary
    Positive p  <- arbitrary
    MEOE (mu *~ (kilo meter ^ pos3 / second ^ pos2)) (p *~ meter)
         <$> arbitrary1 <*> arbitrary1 <*> arbitrary1 <*> arbitrary1 <*> arbitrary
    where arbitrary1 = (*~one) . snd . properFraction <$> arbitrary

-- instance Arbitrary a => Arbitrary (SV a) where arbitrary = (,) <$> arbitrary <*> arbitrary

deriving instance AEq a => AEq (SemiMajorAxis a)
deriving instance AEq a => AEq (SemiLatusRectum a)
deriving instance AEq a => AEq (Eccentricity a)

instance (RealFloat a, Eq a) => Eq (Anomaly t a) where
  (Anom x) == (Anom y) = plusMinusPi x == plusMinusPi y

instance (RealFloat a, AEq a) => AEq (Anomaly t a) where
  (Anom x) ~== (Anom y) = plusMinusPi x ~== plusMinusPi y
                       ||   zeroTwoPi x ~==   zeroTwoPi y  -- move the boundaries.

instance (RealFloat a, Eq a) => Eq (Longitude l a) where
  (Long x) == (Long y) = plusMinusPi x == plusMinusPi y

instance (RealFloat a, AEq a) => AEq (Longitude l a) where
  (Long x) ~== (Long y) = plusMinusPi x ~== plusMinusPi y
                       ||   zeroTwoPi x ~==   zeroTwoPi y  -- move the boundaries.

deriving instance (RealFloat a,  Eq a) =>  Eq (MEOE l a)
--deriving instance (RealFloat a, AEq a) => AEq (MEOE l a)
