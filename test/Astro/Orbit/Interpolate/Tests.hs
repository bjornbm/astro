{-# LANGUAGE ScopedTypeVariables #-}

module Astro.Orbit.Interpolate.Tests (main, specs) where

import Test.Hspec
import Test.QuickCheck (property, (==>), NonZero(..))
import Data.AEq

import Astro.Time
import Astro.Time.At
import TestUtil
import TestInstances
import Astro.Orbit.Types
import Astro.Orbit.Interpolate
import Astro.Trajectory (Datum)
import Astro.Orbit.MEOE

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra.PosVel (SPos)
import qualified Prelude


type D = Double

main = hspec specs
specs = do
  spec_linearPolate
  spec_linearPolateT
  spec_linearPolateVec
  spec_linearPolateVecT
  spec_linearPolateMEOEm
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


spec_linearPolateMEOEm = describe "linearPolateMEOEm (m0`At`t0) (m1`At`t1) t" $ do

  it "returns m0 when interpolating at t0." $ property $
    \(m0`At`t0::Datum UT1 D) (m1`At`t1) -> t0 /= t1
      ==> linearPolateMEOEm (m0`At`t0) (m1`At`t1) t0 == m0

  it "returns m1 when interpolating at t1." $ property $
    \(m0`At`t0::Datum UT1 D) (m1`At`t1) -> t0 /= t1
      ==> linearPolateMEOEm (m0`At`t0) (m1`At`t1) t1 ~== m1

  it "No2 returns m0 when interpolating at t0."  $ property $
    \(m0`At`t0::Datum UT1 D) (m1`At`t1) -> t0 /= t1
      ==> linearPolateMEOEm2 (m0`At`t0) (m1`At`t1) t0 == m0

  it "No2 returns m1 when interpolating at t1." $ property $
    \(m0`At`t0::Datum UT1 D) (m1`At`t1) -> t0 /= t1
      ==> linearPolateMEOEm2 (m0`At`t0) (m1`At`t1) t1 ~== m1

  it "No2 is equivalent." $ property $
    \(m0`At`t0::Datum UT1 D) (m1`At`t1) t -> t0 /= t1
      ==> linearPolateMEOEm2 (m0`At`t0) (m1`At`t1) t
      ~== linearPolateMEOEm (m0`At`t0) (m1`At`t1) t

  it "works for a previously failing test case" $
    let m0 = At {value = MEOE { mu = (196.85310683970377)*~(meter^pos3/second^pos2)
                 , p = (163.53701304688119)*~meter
                 , f = (0.7624710769118792)*~one
                 , g = (0.46455179371666233)*~one
                 , h = (-0.9178421033438866)*~one
                 , k = (-0.807264463290684)*~one
                 , longitude = Long {long = (-588.9134233811517)*~one}}
                 , epoch = clock 1858 10 31 23 44 24.763022427302 UT1}
        m1 = At {value = MEOE {mu = 91.52550514126447*~(meter^pos3/second^pos2)
                 , p = 0.2706275352248937*~meter
                 , f = 0.7158444695473349*~one
                 , g = 5.598471234366187e-2*~one
                 , h = 0.4282501853485172*~one
                 , k = 0.8821696262639556*~one
                 , longitude = Long {long = (-45.47333651254473)*~one}}
                 , epoch = clock 1845 10 30 18 02 52.622930313809 UT1::E UT1 D}
    in linearPolateMEOEm m0 m1 (epoch m1) ~== value m1

-- {-
  it "A failing test case due to degenerate numerics" $
    let m0 = At {value = (MEOE {mu = (88.26019212890891)*~(meter^pos3/second^pos2), p = (60.74053490573347)*~meter, f = (-0.10287594643823184)*~one, g = (0.33022867608059414)*~one, h = (0.566530781675775)*~one, k = (-0.9535070415326317)*~one, longitude = Long {long = (-136.19321025953184)*~one}}), epoch = clock 1859 02 25 14 31 58.664186554497 UT1}
        m1 = At {value = (MEOE {mu = (28.347782649561204)*~(meter^pos3/second^pos2), p = (81.31848224944541)*~meter, f = (0.14929490687671887)*~one, g = (-0.1494207496036637)*~one, h = (0.4877418113629943)*~one, k = (0.2923091817478394)*~one, longitude = Long {long = 226.1964600997837*~one}}), epoch = clock 1858 08 08 12 29 13.897002466374 UT1}
    in linearPolateMEOEm2 m0 m1 (epoch m1) ~== (value m1 :: MEOE Mean D)
-- -}

-- ---------------------------------------------------------------------

spec_adjustCyclic = describe "adjustCyclic1 (x0,y0) (x1,y1) period cycle" $ do

  it "seems to work correctly" $
    adjustCyclic (_0, negate _3) (_1, _3) _1 (_2 * pi) `shouldBe` _3

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

