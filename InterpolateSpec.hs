{-# LANGUAGE ScopedTypeVariables #-}

import Test.Hspec.Monadic
import Test.Hspec.QuickCheck (property)
import Test.QuickCheck ((==>))
import Data.AEq

-- import Web.AstroPosVel (Datum)
import Astro.Time
import TestUtil
import Interpolate

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra.PosVel
import qualified Prelude


type D = Double

main = do
  hspec spec_linearPolate
  hspec spec_linearPolateT
  hspec spec_linearPolateVec
  hspec spec_linearPolateVecT
  hspec spec_polateDatum
  hspec spec_polateDatum2


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
      ==> linearPolateT (t0,y0) (t1,y1) t0 ~== y0)

  it "returns y1 when interpolating at t1."
    (property $ \(t0::E UT1 D, y0::Length D) (t1,y1) -> t0 /= t1
      ==> linearPolateT (t0,y0) (t1,y1) t1 ~== y1)


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
      ==> linearPolateVecT (t1, x1) (t2, x2) t1 ~== x1)

  it "Interpolating at the end time returns the end value"
    (property $ \(t1::E UT1 D) (x1::SPos D) t2 x2 -> t1 /= t2
      ==> linearPolateVecT (t1, x1) (t2, x2) t2 ~== x2)


spec_polateDatum = describe "Interpolate.polateDatum" $ do

  it "Interpolating at the start time returns the start datum"
    (property $ \(d1::Datum D) d2 -> fst d1 /= fst d2 ==> polateDatum d1 d2 (fst d1) ~== d1)

  it "Interpolating at the end time returns the end datum"
    (property $ \(d1::Datum D) d2 -> fst d1 /= fst d2 ==> polateDatum d1 d2 (fst d2) ~== d2)


spec_polateDatum2 = describe "Interpolate.polateDatum2" $ do

  it "Interpolating at the start time returns the start datum"
    (property $ \(d1::Datum D) d2 -> fst d1 /= fst d2 ==> polateDatum2 d1 d2 (fst d1) ~== d1)

  it "Interpolating at the end time returns the end datum"
    (property $ \(d1::Datum D) d2 -> fst d1 /= fst d2 ==> polateDatum2 d1 d2 (fst d2) ~== d2)
