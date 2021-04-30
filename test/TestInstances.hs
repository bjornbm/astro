{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}


module TestInstances where

import Control.Applicative
import Control.Monad (replicateM)
import Test.QuickCheck hiding (property)-- (Arbitrary, arbitrary, (==>))
import Data.AEq
import Data.Proxy
import GHC.TypeLits
import TestUtil
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Coercion
import Numeric.Units.Dimensional (Dimensional (..))
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.Vector (Vec (ListVec))
import Numeric.Units.Dimensional.LinearAlgebra.PosVel (Sph (..))
import qualified Prelude
import Astro.Util.Cyclic (plusMinusPi, zeroTwoPi)
import Astro.Coords
import Astro.Coords.PosVel
import Astro.Place
import Astro.Place.ReferenceEllipsoid
import Astro.Orbit.Types
import Astro.Orbit.MEOE as M -- (MEOE (MEOE), meoe2vec)
import qualified Astro.Orbit.COE as C -- (COE (COE), coe2vec)
import Astro.Orbit.Conversion (meoe2coe)
import Astro.Orbit.Maneuver
import Astro.Time hiding (coerce)
import Astro.Time.At


-- ----------------------------------------------------------
-- Special generators and Arbitrary instances.

-- These could be defined in terms of the newtypes, e,g, getNonZeroD <$> arbitrary
nonZeroArbitrary :: (Arbitrary a, Eq a, Num a) => Gen (Quantity d a)
nonZeroArbitrary = suchThat arbitrary (/= _0)
positiveArbitrary :: (Arbitrary a, Ord a, Num a) => Gen (Quantity d a)
positiveArbitrary = suchThat arbitrary (> _0)
nonNegativeArbitrary :: (Arbitrary a, Ord a, Num a) => Gen (Quantity d a)
nonNegativeArbitrary = suchThat arbitrary (>= _0)
zeroOneArbitrary :: (Arbitrary a, RealFrac a) => Gen (Dimensionless a)
zeroOneArbitrary = (*~one) . snd . properFraction <$> arbitrary

-- | @NonZeroD x@ has an Arbitrary instance that guarantees that @x \/= 0@.
newtype NonZeroD d a = NonZeroD { getNonZeroD :: Quantity d a }
instance (Arbitrary a, Eq a, Num a) => Arbitrary (NonZeroD d a) where
  arbitrary = NonZeroD <$> suchThat arbitrary (/= _0)
deriving instance (KnownDimension d, Real a, Show a) => Show (NonZeroD d a)

-- | @PositiveD x@ has an Arbitrary instance that guarantees that @x \> 0@.
newtype PositiveD d a = PositiveD { getPositiveD :: Quantity d a }
instance (Arbitrary a, Ord a, Num a) => Arbitrary (PositiveD d a) where
  arbitrary = PositiveD <$> suchThat arbitrary (> _0)
deriving instance (KnownDimension d, Real a, Show a) => Show (PositiveD d a)

-- | @NonNegativeD x@ has an Arbitrary instance that guarantees that @x \>= 0@.
newtype NonNegativeD d a = NonNegativeD { getNonNegativeD :: Quantity d a }
instance (Arbitrary a, Ord a, Num a) => Arbitrary (NonNegativeD d a) where
  arbitrary = NonNegativeD <$> suchThat arbitrary (>= _0)
deriving instance (KnownDimension d, Real a, Show a) => Show (NonNegativeD d a)

-- | @ZeroOneD x@ has an Arbitrary instance that guarantees that @0 <= x < 1@.
newtype ZeroOneD a = ZeroOneD { getZeroOneD :: Dimensionless a } deriving (Show)
instance (Arbitrary a, RealFrac a) => Arbitrary (ZeroOneD a) where
  arbitrary = ZeroOneD . (*~one) . snd . properFraction <$> arbitrary

-- ----------------------------------------------------------
-- Arbitrary instances
-- -------------------

instance (Arbitrary a) => Arbitrary (Quantity d a) where
  arbitrary = coerceQ <$> arbitrary
    where coerceQ = coerce :: a -> Quantity d a

instance (KnownNat n, Arbitrary a) => Arbitrary (Vec d n a) where
  arbitrary = fromListErr <$> vectorOf n arbitrary
    where
      n = fromInteger $ natVal (Proxy :: Proxy n)

instance Arbitrary a => Arbitrary (Coord s a) where
  arbitrary = C <$> arbitrary

-- | Guarantees the vector (or @Coord@) is non-null.
newtype NonNull v = NonNull v deriving (Show)

instance Functor NonNull where fmap f (NonNull v) = NonNull (f v)

instance (KnownNat n, Num a, Eq a, Arbitrary a) => Arbitrary (NonNull (Vec d n a)) where
  arbitrary = NonNull <$> suchThat arbitrary (/= nullVector)

instance (Num a, Eq a, Arbitrary a) => Arbitrary (NonNull (Coord s a)) where
  arbitrary = fmap C <$> arbitrary


instance Arbitrary a => Arbitrary (GeodeticLatitude a) where
  arbitrary = GeodeticLatitude <$> arbitrary

instance Arbitrary a => Arbitrary (GeoLongitude a) where
  arbitrary = GeoLongitude <$> arbitrary

instance (Fractional a, Ord a, Arbitrary a) => Arbitrary (GeodeticPlace a) where
  arbitrary = GeodeticPlace <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Num a, Ord a, Arbitrary a) => Arbitrary (ReferenceEllipsoid a) where
  arbitrary = do
    x <- positiveArbitrary
    y <- positiveArbitrary
    return $ ReferenceEllipsoid (max x y) (min x y)

instance (Arbitrary a, Fractional a) => Arbitrary (E t a) where
  arbitrary = mjd' <$> arbitrary

instance (Arbitrary a, Fractional a, AEq a) => Arbitrary (PosVel s a) where
  arbitrary = do
      let pv = C' <$> arbitrary <*> arbitrary
      suchThat pv (not . degeneratePosVel)

deriving instance Arbitrary a => Arbitrary (SemiMajorAxis a)
deriving instance Arbitrary a => Arbitrary (SemiLatusRectum a)
deriving instance Arbitrary a => Arbitrary (Anomaly t a)
deriving instance Arbitrary a => Arbitrary (Longitude t a)

-- Arbitrary instance always returns values >= 0.
instance (Num a, Ord a, Arbitrary a) => Arbitrary (Eccentricity a) where
  arbitrary = Ecc <$> nonNegativeArbitrary

instance Arbitrary a => Arbitrary (Maneuver a) where
  arbitrary = ImpulsiveRTN <$> arbitrary <*> arbitrary <*> arbitrary

-- This instance will not generate orbits with very large eccentricities.
instance (RealFrac a, Ord a, Arbitrary a) => Arbitrary (M.MEOE t a) where
  arbitrary = do
    let m = M.MEOE <$> positiveArbitrary
                   <*> positiveArbitrary
                   <*> zeroOneArbitrary <*> zeroOneArbitrary
                   <*> zeroOneArbitrary <*> zeroOneArbitrary
                   <*> arbitrary
    suchThat m (\m -> semiMajorAxis m > SMA _0)

instance (RealFloat a, Arbitrary a) => Arbitrary (C.COE t a) where
  arbitrary = meoe2coe <$> arbitrary

instance (Fractional a, Arbitrary a, Arbitrary x) => Arbitrary (At t a x) where
  arbitrary = At <$> arbitrary <*> arbitrary

-- ----------------------------------------------------------
-- AEq instances.

-- Approximate equality
-- --------------------

instance (RealFloat a, AEq a) => AEq (E t a) where
  E t1 ~== E t2 = t1 ~== t2

instance (RealFloat a, AEq a) => AEq (Sph d a) where
  Sph r1 zen1 ra1 ~== Sph r2 zen2 ra2 = r1 ~== r2 && zen1 ~== zen2 && ra1 ~== ra2

deriving instance AEq a => AEq (SemiMajorAxis a)
deriving instance AEq a => AEq (SemiLatusRectum a)
deriving instance AEq a => AEq (Eccentricity a)

instance (RealFloat a, Eq a) => Eq (Anomaly t a) where
  Anom x == Anom y = x ==~ y  -- TODO Cyclic may be good, but also approximate for Eq??

instance (RealFloat a, AEq a) => AEq (Anomaly t a) where
  Anom x ~== Anom y = x ~==~ y

instance (RealFloat a, Eq a) => Eq (Longitude l a) where
  Long x == Long y = x ==~ y  -- TODO Cyclic may be good, but also approximate for Eq??

instance (RealFloat a, AEq a) => AEq (Longitude l a) where
  Long x ~== Long y = x ~==~ y

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
           && C.aop  c0 ~==~ C.aop  c1
           && C.raan c0 ~==~ C.raan c1
           && anom (C.anomaly c0) ~==~ anom (C.anomaly c1)

instance (RealFloat a, AEq a, AEq x) => AEq (At t a x) where
  (x0 `At` t0) ~== (x1 `At` t1) = x0 ~== x1 && t0 ~== t1
