{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}


module Astro.Orbit.Types where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (revolution)
import qualified Prelude
import Data.AEq
import TestUtil
import Test.QuickCheck


newtype SemiMajorAxis a = SMA { sma :: Length a } deriving (Show, Eq, AEq, Arbitrary)  -- Should be positive?
newtype SemiLatusRectum a = SLR { slr :: Length a } deriving (Show, Eq, AEq, Arbitrary)  -- Should be positive.

-- | Eccentricity. Should be >= 0.
newtype Eccentricity a = Ecc { ecc :: Dimensionless a } deriving (Show, Eq, AEq)

-- Arbitrary instance always returns values >= 0.
instance (Num a, Ord a, Arbitrary a) => Arbitrary (Eccentricity a) where
    arbitrary = do
      NonNegative e <- arbitrary
      return $ Ecc (e*~one)


-- Angles
-- ======
-- For angles the following should hold: x == x + n 2 pi

-- | True Anomaly.
newtype TrueAnomaly a = TA { ta :: Angle a } deriving (Show, Arbitrary)

instance (RealFloat a, Eq a) => Eq (TrueAnomaly a) where
  (TA x) == (TA y) = plusMinusPi x == plusMinusPi y

instance (RealFloat a, AEq a) => AEq (TrueAnomaly a) where
  (TA x) ~== (TA y) = plusMinusPi x ~== plusMinusPi y
                   || plusTwoPi x ~== plusTwoPi y  -- move the boundaries.

-- | Eccentric Anomaly.
newtype EccentricAnomaly a = EA { ea :: Angle a } deriving (Show, Arbitrary)

instance (RealFloat a, Eq a) => Eq (EccentricAnomaly a) where
  (EA x) == (EA y) = plusMinusPi x == plusMinusPi y

instance (RealFloat a, AEq a) => AEq (EccentricAnomaly a) where
  (EA x) ~== (EA y) = plusMinusPi x ~== plusMinusPi y
                   || plusTwoPi x ~== plusTwoPi y  -- move the boundaries.

-- | Mean Anomaly.
newtype MeanAnomaly a = MA { ma :: Angle a } deriving (Show, Arbitrary)

instance (RealFloat a, Eq a) => Eq (MeanAnomaly a) where
  (MA x) == (MA y) = plusMinusPi x == plusMinusPi y

instance (RealFloat a, AEq a) => AEq (MeanAnomaly a) where
  (MA x) ~== (MA y) = plusMinusPi x ~== plusMinusPi y
                   || plusTwoPi x ~== plusTwoPi y  -- move the boundaries.


-- Helpers
-- =======

-- | @normalizeAngle center a@ normalizes the angle @a@ to
-- be within ±π of @center@. Algorithm from:
-- http://www.java2s.com/Tutorial/Java/0120__Development/Normalizeanangleina2piwideintervalaroundacentervalue.htm
normalizeAngle :: RealFloat a => Angle a -> Angle a -> Angle a
normalizeAngle center a = a - _2 * pi * floor' ((a + pi - center) / (_2 * pi))
  where floor' = (*~ one) . fromIntegral . floor . (/~ one)

-- | Constrains an angle to the range [-pi,pi).
plusMinusPi :: RealFloat a => Angle a -> Angle a
plusMinusPi = normalizeAngle _0
-- | Constrains an angle to the range [0,2*pi).
plusTwoPi   :: RealFloat a => Angle a -> Angle a
plusTwoPi   = normalizeAngle pi
