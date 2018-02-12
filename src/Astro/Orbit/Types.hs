{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}

module Astro.Orbit.Types where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude


-- Parameters.
newtype SemiMajorAxis a = SMA { sma :: Length a } deriving (Show, Eq, Ord)
newtype SemiLatusRectum a = SLR { slr :: Length a } deriving (Show, Eq, Ord)
-- | Eccentricity. Should be >= 0.
newtype Eccentricity a = Ecc { ecc :: Dimensionless a } deriving (Show, Eq, Ord)

-- Angles
-- ======

data True
data Mean
data Eccentric

newtype Anomaly t a = Anom { anom :: Angle a } deriving (Show)
newtype Longitude t a = Long { long :: Angle a } deriving (Show)
-- Rename the above the "argument of longitude".
--newtype ArgumentOfLatitude  t a = AoLat { argLat :: Angle a } deriving (Show)
--newtype ArgumentOfLongitude t a = AoLon { argLon :: Angle a } deriving (Show)
--newtype ArgumentOfPerigee   t a = AoP   { app    :: Angle a } deriving (Show)


-- Maneuvers
-- =========

data Maneuver a = ImpulsiveRTN { dvr :: Velocity a
                               , dvt :: Velocity a
                               , dvn :: Velocity a
                               } deriving (Show, Eq)
