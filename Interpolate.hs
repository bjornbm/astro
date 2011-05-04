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


-- dimensional
-- -----------
-- Interpolate.
linearPolate :: (Div d d DOne, Mul DOne dy dy, Fractional a)
             => (Quantity d a, Quantity dy a) -> (Quantity d a, Quantity dy a)
             -> Quantity d a -> Quantity dy a
linearPolate (x0,y0) (x1,y1) x = y0 + ((x - x0) / (x1 - x0)) * (y1 - y0)


-- dimensional-vectors
-- -------------------
-- Interpolate vector.
linearPolateVec :: (Div d d DOne, Fractional a)
                => (Quantity d a, Vec ds a) -> (Quantity d a, Vec ds a)
                -> Quantity d a -> Vec ds a
linearPolateVec (x0,v0) (x1,v1) x = v0 `elemAdd` scaleVec1 ((x - x0) / (x1 - x0)) (v1 `elemSub` v0)


-- Depend on astro
-- ---------------

-- | Interpolate or extrapolate linearly.
linearPolateT :: (Mul DOne d d, Fractional a)
              => (E t a, Quantity d a) -> (E t a, Quantity d a)
              -> E t a -> Quantity d a
linearPolateT (t1, x1) (t2, x2) t = x1 + diffEpoch t t1 / diffEpoch t2 t1 * (x2 - x1)

-- Interpolate vector as function of time.
linearPolateVecT :: Fractional a
                 => (E t a, Vec ds a) -> (E t a, Vec ds a) -> E t a -> Vec ds a
linearPolateVecT (t1,v1) (t2,v2) t = linearPolateVec (f t1,v1) (f t2,v2) (f t)
  where f t = diffEpoch t (mjd' 0)


-- Interpolate datum. Soon to be removed.

polateDatum :: RealFloat a => Datum a -> Datum a -> E UT1 a -> Datum a
polateDatum (t1,pv1) (t2,pv2) t = (t,pv)
  where
    ( x1, y1, z1) = toTuple $ cpos pv1
    ( x2, y2, z2) = toTuple $ cpos pv2
    (vx1,vy1,vz1) = toTuple $ cvel pv1
    (vx2,vy2,vz2) = toTuple $ cvel pv2
    x = linearPolateT (t1,x1) (t2,x2) t
    y = linearPolateT (t1,y1) (t2,y2) t
    z = linearPolateT (t1,z1) (t2,z2) t
    vx = linearPolateT (t1,vx1) (t2,vx2) t
    vy = linearPolateT (t1,vy1) (t2,vy2) t
    vz = linearPolateT (t1,vz1) (t2,vz2) t
    pv = C' (fromTuple (x,y,z)) (fromTuple (vx,vy,vz))


polateDatum2 :: RealFloat a => Datum a -> Datum a -> E UT1 a -> Datum a
polateDatum2 d1 d2 t = (t, C' p v)
  where
    p = linearPolateVecT (fmap cpos d1) (fmap cpos d2) t
    v = linearPolateVecT (fmap cvel d1) (fmap cvel d2) t
