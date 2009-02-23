This module provides forwards automatic differentiation for quantities.

> {-# OPTIONS_GHC -fglasgow-exts #-}

> module Tmp.ForwardAD (diff) where

> import Numeric.Units.Dimensional (Dimensional (Dimensional), Quantity, Div)
> import Tmp.Fad (Dual, diffUU)

> diff :: (Num a, Div d2 d1 d2')
>   => (forall tag. Quantity d1 (Dual tag a) -> Quantity d2 (Dual tag a))
>   -> Quantity d1 a -> Quantity d2' a
> diff f (Dimensional x) = Dimensional (diffUU f' x)
>   where
>     f' = undim . f . Dimensional
>     undim (Dimensional a) = a

