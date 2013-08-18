> module Astro.Functions where

> import qualified Prelude
> import Control.Monad.Reader
> import Astro
> import Astro.Coords.PosVel
> import Numeric.Units.Dimensional.Prelude
> import Numeric.Units.Dimensional.NonSI (revolution)


| The length of a sidereal day.

> siderealDay :: Floating a => Astro a (Time a)
> siderealDay = do
>   phi <- asks phi
>   return $ 1 *~ revolution / phi


| Calculates the potential energy per unit mass of a body at the given distance from the center of Earth.

> potentialEnergyPerUnitMass :: Floating a => Length a -> Astro a (EnergyPerUnitMass a)
> potentialEnergyPerUnitMass r = do
>   mu <- asks mu
>   return $ negate mu / r

| Calculates the total orbital energy per unit mass of a body with the
given @PosVel@. The @PosVel@ must be in an inertial reference frame.

> orbitalEneryPerUnitMass :: Floating a => PosVel s a -> Astro a (EnergyPerUnitMass a)
> orbitalEneryPerUnitMass pv = do
>   mu <- asks mu
>   return $ (dotProduct v v) / _2 + mu / r
>   where 
>     r = radius (spos pv)
>     v = (cvel pv)


> longitudeToRA :: Fractional a => Epoch -> Longitude a -> Astro a (RightAscension a)
> longitudeToRA t l = do
>       ra0 <- asks greenwichRefRA
>       t0  <- asks greenwichRefEpoch
>       phi <- asks phi
>       return $ ra0 + phi * (diffTime t t0) + l


