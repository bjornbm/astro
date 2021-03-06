{-# LANGUAGE RankNTypes #-}

module Astro.Coords.PosVel where

import Astro.Coords
import Astro.Time (E (E), addTime)  -- For the Lift instance.
import Astro.Time.At (At (At))
import Numeric.Units.Dimensional.Prelude (_0)  -- for liftPVAt.
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.PosVel
import Numeric.Units.Dimensional.LinearAlgebra.VectorAD (applyLinear, applyLinearAt)
import Astro.AD
import Data.AEq

-- $setup
-- >>> import TestInstances

data PosVel system a = C' (CPos a) (CVel a)
                     | S' (SPos a) (SVel a)
                     deriving (Show, Eq)

-- Cannot derive AEq instance since can have both CPos and SPos.
instance (RealFloat a, AEq a) => AEq (PosVel s a) where
  pv1 ~== pv2 = cpos pv1 ~== cpos pv2 && cvel pv1 ~== cvel pv2

pos :: PosVel s a -> Coord s a
pos (C' p _) = C p
pos (S' p _) = S p

cpos :: Floating a => PosVel s a -> CPos a
cpos (C' p _) = p
cpos (S' p _) = s2c p

spos :: RealFloat a => PosVel s a -> SPos a
spos (S' p _) = p
spos (C' p _) = c2s p

-- | Extract the cartesian velocity vector.
cvel :: RealFloat a => PosVel s a -> CVel a
cvel (C' _ v) = v
cvel (S' p v) = snd $ s2cEphem (p,v)

-- | Extract the spherical velocity.
svel :: RealFloat a => PosVel s a -> SVel a
svel (S' _ v) = v
svel (C' p v) = snd $ c2sEphem (p,v)

-- | Returns true if the state vector is degenerate. A state vector is
-- degenerate if the angular momentum (about the central body/origo) is
-- zero.
--
-- prop> degeneratePosVel (C' nullVector v)
-- prop> degeneratePosVel (C' p nullVector)
degeneratePosVel :: (Num a, AEq a) => PosVel s a -> Bool
degeneratePosVel (C' pos vel) = (pos `crossProduct` vel) ~== nullVector



-- | Lift a function on @Coord@s to a function on 'PosVel's.
liftPV :: RealFloat a
       => (forall tag. Coord s (FAD tag a) -> Coord s' (FAD tag a))
       -> PosVel s a -> PosVel s' a
liftPV f pv = uncurry C' $ applyLinear (c . f . C) (cpos pv, cvel pv)

-- | Lift a function of time on @Coord@s to a function of time on 'PosVel's.
liftPVAt :: (RealFloat a)
         => (forall tag. E t (FAD tag a) -> Coord s (FAD tag a) -> Coord s' (FAD tag a))
         -> E t a -> PosVel s a -> PosVel s' a
liftPVAt f t pv = uncurry C' $ applyLinearAt
                                (\dt -> c . f (addTime (lift t) dt) . C)
                                _0
                                (cpos pv, cvel pv)

-- | Lift a function of time on @Coord@s to a function of time on 'PosVel's.
liftPVAt' :: (RealFloat a)
         => (forall tag. E t (FAD tag a) -> Coord s (FAD tag a) -> Coord s' (FAD tag a))
         -> At t a (PosVel s a) -> At t a (PosVel s' a)
liftPVAt' f (pv `At` t) = liftPVAt f t pv `At` t
