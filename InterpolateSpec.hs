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
  hspec spec_polate
  hspec spec_polateVec
  hspec spec_polateVec'
  hspec spec_polateDatum
  hspec spec_polateDatum2


spec_polate = describe "Interpolate.polate" $ do

  it "Interpolating at the start time returns the start value"
    (property $ \(t1::E UT1 D) (x1::Length D) t2 x2 -> t1 /= t2 ==> polate (t1, x1) (t2, x2) t1 ~== x1)

  it "Interpolating at the end time returns the end value"
    (property $ \(t1::E UT1 D) (x1::Length D) t2 x2 -> t1 /= t2 ==> polate (t1, x1) (t2, x2) t2 ~== x2)


spec_polateVec = describe "Interpolate.polateVec" $ do

  it "Interpolating at the start returns the start value"
    (property $ \(t1::Time D) (x1::SPos D) t2 x2 -> t1 /= t2 ==> polateVec (t1, x1) (t2, x2) t1 ~== x1)

  it "Interpolating at the end returns the end value"
    (property $ \(t1::Time D) (x1::SPos D) t2 x2 -> t1 /= t2 ==> polateVec (t1, x1) (t2, x2) t2 ~== x2)


spec_polateVec' = describe "Interpolate.polateVec'" $ do

  it "Interpolating at the start time returns the start value"
    (property $ \(t1::E UT1 D) (x1::SPos D) t2 x2 -> t1 /= t2 ==> polateVec' (t1, x1) (t2, x2) t1 ~== x1)

  it "Interpolating at the end time returns the end value"
    (property $ \(t1::E UT1 D) (x1::SPos D) t2 x2 -> t1 /= t2 ==> polateVec' (t1, x1) (t2, x2) t2 ~== x2)


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
