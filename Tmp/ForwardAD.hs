-- This module provides forwards automatic differentiation for quantities.

{-# OPTIONS_GHC -fglasgow-exts #-}

module Tmp.ForwardAD (diff, Lift, lift) where

import Numeric.Units.Dimensional (Dimensional (Dimensional), Quantity, Div)
import Fad (Dual)
import qualified Fad (diff, lift)

diff :: (Num a, Div d2 d1 d2')
     => (forall tag. Quantity d1 (Dual tag a) -> Quantity d2 (Dual tag a))
     -> Quantity d1 a -> Quantity d2' a
diff f (Dimensional x) = Dimensional (Fad.diff f' x)
  where
    f' = undim . f . Dimensional
    undim (Dimensional a) = a


class Lift w where lift :: Num a => w a -> w (Dual tag a)
instance Lift (Dimensional v d) where lift (Dimensional x) = Dimensional (Fad.lift x)


