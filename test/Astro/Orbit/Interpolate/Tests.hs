{-# LANGUAGE ScopedTypeVariables #-}

module Astro.Orbit.Interpolate.Tests (main, specs) where

import Test.Hspec.Monadic
import Test.Hspec.QuickCheck (property)
import Test.QuickCheck ((==>), Positive(..))
import Data.AEq

import Astro.Time
import TestUtil
import Astro.Orbit.Types
import Astro.Orbit.Interpolate

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra.PosVel
import qualified Prelude


type D = Double

main = hspec specs
specs = do
  spec_linearPolate
  spec_linearPolateT
  spec_linearPolateVec
  spec_linearPolateVecT
  spec_adjustCyclic


spec_linearPolate = describe "linearPolate (x0,y0) (x1,y1) x" $ do

  it "returns y0 when interpolating at x0."
    (property $ \(x0::Time D, y0::Length D) (x1,y1) -> x0 /= x1
      ==> linearPolate (x0,y0) (x1,y1) x0 ~== y0)

  it "returns y1 when interpolating at x1."
    (property $ \(x0::Time D, y0::Length D) (x1,y1) -> x0 /= x1
      ==> linearPolate (x0,y0) (x1,y1) x1 ~== y1)

  it "returns y ∈ (y0,y1) iff t ∈ (x0,x1)."
    (property $ \(x0::Time D, y0::Length D) (x1,y1) x -> x0 /= x1
      ==> let y = linearPolate (x0,y0) (x1,y1) x
          in within x0 x1 x `xor` not (within y0 y1 y))

  where
    xor a b = a && not b || b && not a
    within x0 x1 x = x0 < x && x < x1 || x1 < x && x < x0


spec_linearPolateT = describe "linearPolateT (t0,y0) (t1,y1) t" $ do

  it "returns y0 when interpolating at t0."
    (property $ \(t0::E UT1 D, y0::Length D) (t1,y1) -> t0 /= t1
      ==> linearPolateT (y0`At`t0) (y1`At`t1) t0 ~== y0)

  it "returns y1 when interpolating at t1."
    (property $ \(t0::E UT1 D, y0::Length D) (t1,y1) -> t0 /= t1
      ==> linearPolateT (y0`At`t0) (y1`At`t1) t1 ~== y1)


spec_linearPolateVec = describe "Interpolate.linearPolateVec" $ do

  it "Interpolating at the start returns the start value"
    (property $ \(t1::Time D) (x1::SPos D) t2 x2 -> t1 /= t2
      ==> linearPolateVec (t1, x1) (t2, x2) t1 ~== x1)

  it "Interpolating at the end returns the end value"
    (property $ \(t1::Time D) (x1::SPos D) t2 x2 -> t1 /= t2
      ==> linearPolateVec (t1, x1) (t2, x2) t2 ~== x2)


spec_linearPolateVecT = describe "Interpolate.polateVecT" $ do

  it "Interpolating at the start time returns the start value"
    (property $ \(t1::E UT1 D) (x1::SPos D) t2 x2 -> t1 /= t2
      ==> linearPolateVecT (x1`At`t1) (x2`At`t2) t1 ~== x1)

  it "Interpolating at the end time returns the end value"
    (property $ \(t1::E UT1 D) (x1::SPos D) t2 x2 -> t1 /= t2
      ==> linearPolateVecT (x1`At`t1) (x2`At`t2) t2 ~== x2)


-- ---------------------------------------------------------------------

spec_adjustCyclic = describe "adjustCyclic1 (x0,y0) (x1,y1) period cycle" $ do

  it "seems to work correctly"
    (adjustCyclic (_0, negate _3) (_1, _3) _1 (_2 * pi) == _3)

  it "seems to work correctly"
    (adjustCyclic (_0, negate _3) (_1,_3) _1 _8 == _3)

  it "seems to work correctly"
    (adjustCyclic (_0, negate _3) (_1,_5) _5 _8 == negate _3)

  it "seems to work correctly"
    (adjustCyclic (_0,_3) (_1, negate _3) _5 _8 == _5)

  it "seems to work correctly"
    (adjustCyclic (_0,_3) (_1, negate _3) _5 _8 == _5)

  it "seems to work correctly"
    (adjustCyclic (_0,_3) (_1, negate _3) _1 _8 == 13*~one)

  it "seems to work correctly on a previously failing case"
    (let (x0,y0) = ((-9.200202975846752)*~one,3.1439390419699795*~one)
         (x1,y1) = ((-36.091591960763786)*~one,(-3.341608661319045)*~one)
         y1' = adjustCyclic1 (x0,y0) (x1,y1)
      in abs ((y1'-y0) - (x1-x0)) < 0.5 *~one)

  it "seems to work correctly on a previously failing case"
    ( let (x0,y0) = (0.36547819268776943*~one, 7.590913372057166*~one)
          (x1,y1) = ((-2.000468137259748)*~one, (-0.18165146326599815)*~one)
          period = 1.9197163741923564 *~one
          cycle  = 64.0 *~one
          y1' = adjustCyclic (x0,y0) (x1,y1) period cycle
      in abs ((y1'-y0)/cycle - (x1-x0)/period) < 0.5*~one)

  it "seems to work with random inputs"
    (property $ \(x0,y0) (x1,y1) -> let y1' = adjustCyclic1 (x0,y0) (x1,y1)
        in abs ((y1' - y0) - (x1 - x0)) < (0.5 *~one::Dimensionless Double))

  it "seems to work with random inputs"
    (property $ \(x0,y0) (x1,y1) (Positive p) (Positive c)
      -> let period = p *~ second :: Time Double
             cycle  = c *~ meter
             y1' = adjustCyclic (x0,y0) (x1,y1) period cycle
          in abs ((y1'-y0)/cycle - (x1-x0)/period) < 0.5 *~one)
