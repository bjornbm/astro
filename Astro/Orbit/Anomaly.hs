module Astro.Orbit.Anomaly where

-- [1] http://mathworld.wolfram.com/KeplersEquation.html

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Data.AEq
import TestUtil

type Eccentricity a     = Dimensionless a
type TrueAnomaly a      = Angle a
type EccentricAnomaly a = Angle a
type MeanAnomaly a      = Angle a


-- | Compute eccentric anomaly from true anomaly using atan2.
ta2ea :: RealFloat a => Eccentricity a -> TrueAnomaly a -> EccentricAnomaly a
ta2ea e ta = atan2 (sqrt (_1 - e ^ pos2) * sin ta) (e + cos ta)
-- | Compute eccentric anomaly from true anomaly using atan.
ta2ea' :: RealFloat a => Eccentricity a -> TrueAnomaly a -> EccentricAnomaly a
ta2ea' e t = _2 * atan (sqrt ((_1 - e) / (_1 + e)) * tan (t / _2))

-- | Compute mean anomaly from eccentric anomaly.
ea2ma :: RealFloat a => Eccentricity a -> EccentricAnomaly a -> MeanAnomaly a
ea2ma e ea = ea - e * sin ea


-- | Compute true anomaly from eccentric anomaly. (Wikipedia)
ea2ta :: RealFloat a => Eccentricity a -> EccentricAnomaly a -> TrueAnomaly a
ea2ta e ea = _2 * atan (sqrt ((_1 + e) / (_1 - e)) * tan (ea / _2))

-- | Compute eccentric anomaly from mean anomaly using Newton's
-- method as shown on [1].
ma2ea :: (RealFloat a, AEq a)
      => Eccentricity a -> MeanAnomaly a -> EccentricAnomaly a
ma2ea e ma = iterateUntil (~==) (keplerStep e ma) ma


-- Kepler's Equation
-- =================

-- | A step in solving Kepler's equation per [1].
keplerStep :: Floating a => Eccentricity a -> MeanAnomaly a
           -> EccentricAnomaly a -> EccentricAnomaly a
keplerStep e ma ea_ = ea_ + (ma + e * sin ea_ - ea_) / (_1 - e * cos ea_)

-- | Iterate a function on its result until the predicate
-- holds true for two subsequent results. When it does return
-- the latter of the results. (Note: will diverge if the
-- predicate is never fulfilled.)
iterateUntil :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterateUntil p f = until p . iterate f
  where
    until p (x1:x2:xs) = if p x1 x2 then x2 else until p (x2:xs)
