{-# LANGUAGE EmptyDataDecls #-}

module Astro.Orbit.Types where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (revolution)
import Astro.Time (E)
import qualified Prelude


-- Parameters.
newtype SemiMajorAxis a = SMA { sma :: Length a } deriving (Show, Eq, Ord)
newtype SemiLatusRectum a = SLR { slr :: Length a } deriving (Show, Eq, Ord)
-- | Eccentricity. Should be >= 0.
newtype Eccentricity a = Ecc { ecc :: Dimensionless a } deriving (Show, Eq, Ord)

-- Angles
-- ======
-- For angles the following should hold: x == x + n 2 pi

data True
data Mean
data Ecc

newtype Anomaly t a = Anom { anom :: Angle a } deriving (Show)
newtype Longitude t a = Long { long :: Angle a } deriving (Show)
