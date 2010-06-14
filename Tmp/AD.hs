-- This module provides forwards automatic differentiation for quantities.

{-# OPTIONS_GHC -fglasgow-exts #-}

module Tmp.AD (diff, Lift, lift) where

import Numeric.Units.Dimensional (Dimensional (Dimensional), Quantity, Div)
import Numeric.AD (AD, Mode)
import qualified Numeric.AD as AD (diff, lift)

diff :: (Num a, Div d2 d1 d2')
     => (forall tag. Mode tag => Quantity d1 (AD tag a) -> Quantity d2 (AD tag a))
     -> Quantity d1 a -> Quantity d2' a
{-diff f (Dimensional x) = Dimensional (Numeric.AD.diff f' x)
  where
    f' = undim . f . Dimensional
    undim (Dimensional a) = a -}
diff f (Dimensional x) = Dimensional $ AD.diff (undim . f . Dimensional) x
  where
    undim (Dimensional a) = a


class Lift w where lift :: (Num a, Mode tag) => w a -> w (AD tag a)
instance Lift (Dimensional v d) where lift (Dimensional x) = Dimensional (AD.lift x)
