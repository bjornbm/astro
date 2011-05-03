{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interpolate where

-- import Web.AstroPosVel (Datum)
-- import Synchronized
import Astro
import Astro.Time
import Astrodynamics
import Astro.Coords (MEGSD)
import Astro.Coords.PosVel
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional (Dimensional (Dimensional))
import Numeric.Units.Dimensional.NonSI
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.PosVel
import qualified Prelude as P

type Datum a = (E UT1 a, PosVel MEGSD a)

{-
toSync :: RealFloat a => GravitationalParameter a -> Datum a -> Elements a
toSync mu (t,pv) = Elements t ml d e i where
  ml = undefined
  d  = 1*~revolution / (orbitPeriod mu pv) - 1*~revolution / siderealDay
  e  = eccVector pv
  i  = incVector pv

-- | Calculates the total orbital energy per unit mass of a body with the
-- given @PosVel@. The @PosVel@ must be in an inertial reference frame.
orbitalEneryPerUnitMass :: RealFloat a
                        => GravitationalParameter a -> PosVel s a
                        -> EnergyPerUnitMass a
orbitalEneryPerUnitMass mu pv = (dotProduct v v) / _2 + mu / r
  where
    r = radius (spos pv)
    v = (cvel pv)

semiMajorAxis' :: RealFloat a
              => GravitationalParameter a -> PosVel s a
              -> Length a
semiMajorAxis' mu pv = negate mu / (_2 * orbitalEneryPerUnitMass mu pv)

orbitPeriod :: RealFloat a
            => GravitationalParameter a -> PosVel s a
            -> Time a
orbitPeriod mu pv = _2 * pi * sqrt (semiMajorAxis' mu pv ^ pos3 / mu)


driftRate :: RealFloat a => GravitationalParameter a -> PosVel s a -> DriftRate a
driftRate mu pv = undefined where
-}

-- F = GMm/r^2


-- convert 

-- | Interpolate or extrapolate linearly.
polate :: (Mul DOne d d, Fractional a) => (E t a, Quantity d a) -> (E t a, Quantity d a) -> E t a -> Quantity d a
polate (t1, x1) (t2, x2) t = x1 + diffEpoch t t1 / diffEpoch t2 t1 * (x2 - x1)

polateDatum :: RealFloat a => Datum a -> Datum a -> E UT1 a -> Datum a
polateDatum (t1,pv1) (t2,pv2) t = (t,pv)
  where
    ( x1, y1, z1) = toTuple $ cpos pv1
    ( x2, y2, z2) = toTuple $ cpos pv2
    (vx1,vy1,vz1) = toTuple $ cvel pv1
    (vx2,vy2,vz2) = toTuple $ cvel pv2
    x = polate (t1,x1) (t2,x2) t
    y = polate (t1,y1) (t2,y2) t
    z = polate (t1,z1) (t2,z2) t
    vx = polate (t1,vx1) (t2,vx2) t
    vy = polate (t1,vy1) (t2,vy2) t
    vz = polate (t1,vz1) (t2,vz2) t
    pv = C' (fromTuple (x,y,z)) (fromTuple (vx,vy,vz))


polateDatum2 :: RealFloat a => Datum a -> Datum a -> E UT1 a -> Datum a
polateDatum2 d1 d2 t = (t, C' p v)
  where
    p = polateVec' (fmap cpos d1) (fmap cpos d2) t
    v = polateVec' (fmap cvel d1) (fmap cvel d2) t


-- Interpolate vector.
polateVec :: (Div d d DOne, Fractional a)
           => (Quantity d a, Vec ds a) -> (Quantity d a, Vec ds a) -> Quantity d a -> Vec ds a
polateVec (t1,v1) (t2,v2) t = v1 `elemAdd` scaleVec1 ((t - t1) / (t2 - t1)) (v2 `elemSub` v1)


-- Interpolate vector as function of time.
polateVec' :: Fractional a
           => (E t a, Vec ds a) -> (E t a, Vec ds a) -> E t a -> Vec ds a
polateVec' (t1,v1) (t2,v2) t = polateVec (f t1,v1) (f t2,v2) (f t)
  where f t = diffEpoch t (mjd 0 undefined)
