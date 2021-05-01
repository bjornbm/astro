{-# LANGUAGE ScopedTypeVariables #-}

module Astro.Util.CyclicSpec where

import Test.Hspec
import Test.QuickCheck (property, (==>), NonZero(..))
import Data.AEq

import TestUtil
import TestInstances

import Numeric.Units.Dimensional.Cyclic
import Numeric.Units.Dimensional.Prelude
import qualified Prelude

-- TODO These tests should be migrated to dimensional-experimental.

main = hspec spec
spec = do
  spec_fundamentals
  spec_plusMinusPi
  spec_zeroTwoPi
  spec_adjustCyclic


-- | Verify some basic properties not strictly related to orbit representations.
-- Not really related to Types.
spec_fundamentals = describe "Fundamentals" $ do

  it "atan2 y x + pi/2 ~= atan2 x (-y)"
    (property $ \(y::Dimensionless Double) x -> x /= _0 || y /= _0 ==>
      plusMinusPi (atan2 y x + pi / _2) ~== atan2 x (negate y))

  it "fractionalPart works as advertized"
    (property $ \(x::Dimensionless Double) ->
      fractionalPart x >= negate _1 && fractionalPart x < _1)


-- ----------------------------------------------------------
spec_plusMinusPi = describe "plusMinusPi" $ do

  it "plusMinusPi x = x for x in [-pi,pi)"
    (property $ \(x'::Angle Double) ->
      let x = fractionalPart x' * pi in plusMinusPi x ~== x)

  it "plusMinusPi returns values in [-pi,pi)"
    (property $ \x -> plusMinusPi x > negate pi && plusMinusPi x <= (pi::Angle Double))

  it "plusMinusPi x + tau = plusMinusPi x"
    (property $ \x -> plusMinusPi (x + tau) ~== (plusMinusPi x::Angle Double))


-- ----------------------------------------------------------
spec_zeroTwoPi = describe "zeroTwoPi" $ do

  it "zeroTwoPi x = x for x in [0,tau)"
    (property $ \(x'::Angle Double) ->
       let x = adjustZeroOne x' * tau in zeroTwoPi x ~== x)

  it "zeroTwoPi returns values in [0,tau)"
    (property $ \x -> zeroTwoPi x >= _0 && zeroTwoPi x < (tau::Angle Double))

  it "zeroTwoPi x + tau = zeroTwoPi x"
    (property $ \x -> zeroTwoPi (x + tau) ~== (zeroTwoPi x::Angle Double))

  it "zeroTau x = zeroTwoPi x"
    (property $ \(x::Angle Double) -> zeroTwoPi x == zeroTau x)


-- ----------------------------------------------------------
spec_adjustCyclic = describe "adjustCyclic (x0,y0) (x1,y1) period cycle" $ do

  it "seems to work correctly" $
    adjustCyclic (_0, negate _3) (_1, _3) _1 tau `shouldBe` _3

  it "seems to work correctly" $
    adjustCyclic (_0, negate _3) (_1,_3) _1 _8 `shouldBe` _3

  it "seems to work correctly" $
    adjustCyclic (_0, negate _3) (_1,_5) _5 _8 `shouldBe` negate _3

  it "seems to work correctly" $
    adjustCyclic (_0,_3) (_1, negate _3) _5 _8 `shouldBe` _5

  it "seems to work correctly" $
    adjustCyclic (_0,_3) (_1, negate _3) _5 _8 `shouldBe` _5

  it "seems to work correctly" $
    adjustCyclic (_0,_3) (_1, negate _3) _1 _8 `shouldBe` 13*~one

  it "seems to work correctly on a previously failing case" $
    let (x0,y0) = ((-9.200202975846752)*~one,3.1439390419699795*~one)
        (x1,y1) = ((-36.091591960763786)*~one,(-3.341608661319045)*~one)
        y1' = adjustCyclic1 (x0,y0) (x1,y1)
     in abs ((y1'-y0) - (x1-x0)) < 0.5 *~one

  it "seems to work correctly on a previously failing case" $
    ( let (x0,y0) = (0.36547819268776943*~one, 7.590913372057166*~one)
          (x1,y1) = ((-2.000468137259748)*~one, (-0.18165146326599815)*~one)
          period = 1.9197163741923564 *~one
          cycle  = 64.0 *~one
          y1' = adjustCyclic (x0,y0) (x1,y1) period cycle
      in abs ((y1'-y0)/cycle - (x1-x0)/period) < 0.5*~one)

  it "seems to work with random inputs" $ property $
    \(x0,y0) (x1,y1) -> let y1' = adjustCyclic1 (x0,y0) (x1,y1)
        in abs ((y1' - y0) - (x1 - x0)) < (0.5 *~one::Dimensionless D)

  it "seems to work with random inputs" $ property $
    \(x0::Time D,y0::Length D) (x1,y1) (NonZeroD period) (NonZeroD cycle)
      -> let y1' = adjustCyclic (x0,y0) (x1,y1) period cycle
          in abs ((y1'-y0)/cycle - (x1-x0)/period) < 0.5 *~one

  it "always adjusts by an integer number of cycles" $ property $
    \(x0::Time D,y0::Length D) (x1,y1) (NonZeroD period) (NonZeroD cycle)
      -> let y1' = adjustCyclic (x0,y0) (x1,y1) period cycle
             dy1 = (y1' - y1) / cycle /~ one
          in dy1 ~== fromInteger (round dy1)
          -- || y1' ~== y1  -- Catches cases where y1 is not adjusted but
                            -- due to arithmetic changes slightly more than
                            -- epsilon.

  it "now passes a test that failed earlier due to bad numerics" $
         let (x0,y0) = (110.11241870409592 *~second,(-44.69574262947886)*~meter)
             (x1,y1) = (45.78313070344765 *~second,(-60.42668049624092)*~meter)
             period = (-184.186385868482)*~second
             cycle = (-28.31292968999108)*~meter :: Length D
             y1' = adjustCyclic (x0,y0) (x1,y1) period cycle
             dy1 = (y1' - y1) / cycle /~ one
          in dy1 ~== fromInteger (round dy1)
