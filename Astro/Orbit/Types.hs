{-# LANGUAGE EmptyDataDecls #-}

module Astro.Orbit.Types where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (revolution)
import qualified Prelude


newtype SemiMajorAxis a = SMA { sma :: Length a } deriving (Show, Eq)
newtype SemiLatusRectum a = SLR { slr :: Length a } deriving (Show, Eq)
-- | Eccentricity. Should be >= 0.
newtype Eccentricity a = Ecc { ecc :: Dimensionless a } deriving (Show, Eq)

-- Angles
-- ======
-- For angles the following should hold: x == x + n 2 pi

data True
data Mean
data Ecc

newtype Anomaly t a = Anom { anom :: Angle a } deriving (Show)
newtype Longitude t a = Long { long :: Angle a } deriving (Show)


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
