{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE EmptyDataDecls #-}

module Astro.Orbit.Anomaly where

-- [1] http://mathworld.wolfram.com/KeplersEquation.html

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.AEq
import qualified Prelude
import Astro.Orbit.Types
import Data.AEq

-- | Compute eccentric anomaly from true anomaly using atan2.
ta2ea :: RealFloat a => Eccentricity a -> Anomaly True a -> Anomaly Eccentric a
ta2ea Ecc{ecc} Anom{anom} = Anom $ atan2 (sqrt (_1 - ecc ^ pos2) * sin anom) (ecc + cos anom)
-- | Compute eccentric anomaly from true anomaly using atan.
ta2ea' :: RealFloat a => Eccentricity a -> Anomaly True a -> Anomaly Eccentric a
ta2ea' Ecc{ecc} (Anom ta) = Anom $ _2 * atan (sqrt ((_1 - ecc) / (_1 + ecc)) * tan (ta / _2))

-- | Compute mean anomaly from eccentric anomaly.
ea2ma :: RealFloat a => Eccentricity a -> Anomaly Eccentric a -> Anomaly Mean a
ea2ma Ecc{ecc} (Anom ea) = Anom $ ea - ecc * sin ea


-- | Compute true anomaly from eccentric anomaly. (Wikipedia)
ea2ta :: RealFloat a => Eccentricity a -> Anomaly Eccentric a -> Anomaly True a
ea2ta Ecc{ecc} (Anom ea) = Anom $ _2 * atan (sqrt ((_1 + ecc) / (_1 - ecc)) * tan (ea / _2))

-- | Compute eccentric anomaly from mean anomaly using Newton's
-- method as shown on [1].
ma2ea :: (RealFloat a, AEq a) => Eccentricity a -> Anomaly Mean a -> Anomaly Eccentric a
ma2ea ecc ma'@(Anom ma) = Anom $ iterateUntil (~==) (keplerStep ecc ma') ma

-- | Compute true anomaly from mean anomaly using Newton's
-- method as shown on [1].
ma2ta :: (RealFloat a, AEq a) => Eccentricity a -> Anomaly Mean a -> Anomaly True a
ma2ta ecc = ea2ta ecc . ma2ea ecc

-- | Compute mean anomaly from true anomaly.
ta2ma :: RealFloat a => Eccentricity a -> Anomaly True a -> Anomaly Mean a
ta2ma ecc = ea2ma ecc . ta2ea ecc


-- Kepler's Equation
-- =================

-- | A step in solving Kepler's equation per [1].
keplerStep :: Floating a => Eccentricity a -> Anomaly Mean a
           -> Angle a -> Angle a
keplerStep (Ecc ecc) (Anom ma) ea_ = ea_ + (ma + ecc * sin ea_ - ea_) / (_1 - ecc * cos ea_)

-- | Iterate a function on its result until the predicate
-- holds true for two subsequent results. Then returns
-- the latter of the results. (Note: will diverge if the
-- predicate is never fulfilled.)
iterateUntil :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterateUntil p f = until p . iterate f
  where
    until p (x1:x2:xs) = if p x1 x2 then x2 else until p (x2:xs)
