{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Astro.UtilSpec where

import Test.Hspec
import Test.QuickCheck (property, (==>))
import Data.AEq

import TestUtil
import TestInstances

import Numeric.Units.Dimensional.Prelude
import qualified Prelude

import Astro.Util
import Astro.Util.Cyclic
import Astro.Coords
import Astro.Coords.PosVel
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude


main = hspec spec
spec = do
  spec_perfectGEO
  spec_perfectGEO'


spec_perfectGEO = describe "perfectGEO" $ do

  it "has the desired radius."
    (property $ \(l::Angle Double) -> (vElemAt zero $ s $ perfectGEO l) ~== r_GEO)

  it "has the desired longitude."
    (property $ \(l::Angle Double) -> (vElemAt pos2 $ s $ perfectGEO l) ~== plusMinusPi l)

  it "has the desired zenith angle in ECR."
    (property $ \(l::Angle Double) -> (vElemAt pos1 $ s $ perfectGEO l) == tau / _4)

  it "does not have an out-of-plane component (cartesian)."
    (property $ \(l::Angle Double) -> (vElemAt pos2 $ c $ perfectGEO l) == _0)


spec_perfectGEO' = describe "perfectGEO'" $ do

  it "has zero velocity in ECR frame."
    (property $ \(l::Angle Double) -> (vNorm $ cvel $ perfectGEO' l) == _0)

  it "has zero velocity in ECR frame (spherical)."
    (property $ \(l::Angle Double) -> (vElemAt zero $ svel $ perfectGEO' l) == _0)
