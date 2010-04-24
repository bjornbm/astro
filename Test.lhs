#!/usr/bin/env runhaskell

> import qualified Astro.TestTime
> import qualified Astro.TestCelestrak
> import qualified Astro.Place.TestTopocentric
> import qualified IAU2000.TestEquations

> main = do
>   Astro.TestTime.main
>   Astro.TestCelestrak.main
>   Astro.Place.TestTopocentric.main
>   IAU2000.TestEquations.main

