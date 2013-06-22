{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}



module Astro.Orbit.Types.Tests where

import Test.Hspec
import Test.QuickCheck (property, (==>))
import Data.AEq

import TestUtil

import Numeric.Units.Dimensional.Prelude
import qualified Prelude

import Astro.Orbit.Types



main = hspec specs
specs = do
  spec_fundamentals
  spec_plusMinusPi
  spec_zeroTwoPi


-- | Verify some basic properties not strictly related to orbit representations.
-- Not really related to Types.
spec_fundamentals = describe "Fundamentals" $ do

  it "atan2 y x + pi/2 ~= atan2 x (-y)"
    (property $ \(y::Dimensionless Double) x -> x /= _0 || y /= _0 ==>
      plusMinusPi (atan2 y x + pi / _2) ~== atan2 x (negate y))

  it "fractionalPart works as advertized"
    (property $ \x -> fractionalPart x >= _0 && fractionalPart x < _1)


-- ----------------------------------------------------------
spec_plusMinusPi = describe "plusMinusPi" $ do

  it "plusMinusPi -2*pi = 0"
    (plusMinusPi (negate _2 * pi) == _0)

  it "plusMinusPi -pi = -pi"
    (plusMinusPi (negate pi) == negate pi)

  it "plusMinusPi 0 = 0"
    (plusMinusPi _0 == _0)

  it "plusMinusPi pi = -pi"
    (plusMinusPi pi == negate pi)

  it "plusMinusPi 2pi = 0"
    (plusMinusPi (_2*pi) == _0)

  it "plusMinusPi x = x for x in [-pi,pi)"
    (property $ \x' -> let x = fractionalPart x' * _2 * pi - pi in plusMinusPi x ~== x)

  it "plusMinusPi returns values in [-pi,pi)"
    (property $ \x -> plusMinusPi x > negate pi && plusMinusPi x <= (pi::Angle Double))

  it "plusMinusPi x + 2 pi = plusMinusPi x"
    (property $ \x -> plusMinusPi (x + _2 * pi) ~== (plusMinusPi x::Angle Double))


-- ----------------------------------------------------------
spec_zeroTwoPi = describe "zeroTwoPi" $ do

  it "zeroTwoPi -2*pi = 0"
    (zeroTwoPi (negate _2 * pi) == _0)

  it "zeroTwoPi -pi = pi"
    (zeroTwoPi (negate pi) == pi)

  it "zeroTwoPi 0 = 0"
    (zeroTwoPi _0 == _0)

  it "zeroTwoPi pi = pi"
    (zeroTwoPi pi == pi)

  it "zeroTwoPi 2*pi = 0"
    (zeroTwoPi (_2 * pi) == _0)

  it "zeroTwoPi x = x for x in [0,2*pi)"
    (property $ \x' -> let x = fractionalPart x' * _2 * pi in zeroTwoPi x ~== x)

  it "zeroTwoPi returns values in [0,2*pi)"
    (property $ \x -> zeroTwoPi x >= _0 && zeroTwoPi x < (_2 * pi::Angle Double))

  it "zeroTwoPi x + 2 pi = zeroTwoPi x"
    (property $ \x -> zeroTwoPi (x + _2 * pi) ~== (zeroTwoPi x::Angle Double))
