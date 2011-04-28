{-# LANGUAGE NamedFieldPuns #-}
module Astro.Orbit.Anomaly where

-- [1] http://mathworld.wolfram.com/KeplersEquation.html

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro.Orbit.Types
import Data.AEq
import TestUtil


-- | Compute eccentric anomaly from true anomaly using atan2.
ta2ea :: RealFloat a => Eccentricity a -> TrueAnomaly a -> EccentricAnomaly a
ta2ea Ecc{ecc} TA{ta} = EA $ atan2 (sqrt (_1 - ecc ^ pos2) * sin ta) (ecc + cos ta)
-- | Compute eccentric anomaly from true anomaly using atan.
ta2ea' :: RealFloat a => Eccentricity a -> TrueAnomaly a -> EccentricAnomaly a
ta2ea' Ecc{ecc} TA{ta} = EA $ _2 * atan (sqrt ((_1 - ecc) / (_1 + ecc)) * tan (ta / _2))

-- | Compute mean anomaly from eccentric anomaly.
ea2ma :: RealFloat a => Eccentricity a -> EccentricAnomaly a -> MeanAnomaly a
ea2ma Ecc{ecc} EA{ea} = MA $ ea - ecc * sin ea


-- | Compute true anomaly from eccentric anomaly. (Wikipedia)
ea2ta :: RealFloat a => Eccentricity a -> EccentricAnomaly a -> TrueAnomaly a
ea2ta Ecc{ecc} EA{ea} = TA $ _2 * atan (sqrt ((_1 + ecc) / (_1 - ecc)) * tan (ea / _2))

-- | Compute eccentric anomaly from mean anomaly using Newton's
-- method as shown on [1].
ma2ea :: (RealFloat a, AEq a) => Eccentricity a -> MeanAnomaly a -> EccentricAnomaly a
ma2ea ecc ma'@MA{ma} = EA $ iterateUntil (~==) (keplerStep ecc ma') ma


-- Kepler's Equation
-- =================

-- | A step in solving Kepler's equation per [1].
keplerStep :: Floating a => Eccentricity a -> MeanAnomaly a
           -> Angle a -> Angle a
keplerStep Ecc{ecc} MA{ma} ea_ = ea_ + (ma + ecc * sin ea_ - ea_) / (_1 - ecc * cos ea_)

-- | Iterate a function on its result until the predicate
-- holds true for two subsequent results. When it does return
-- the latter of the results. (Note: will diverge if the
-- predicate is never fulfilled.)
iterateUntil :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterateUntil p f = until p . iterate f
  where
    until p (x1:x2:xs) = if p x1 x2 then x2 else until p (x2:xs)
