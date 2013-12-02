{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module AstrodynamicsSpec where

import Test.Hspec
import Test.QuickCheck (property, (==>))
import Data.AEq

import TestUtil
import TestInstances

import Numeric.Units.Dimensional.Prelude
import qualified Prelude

import Astrodynamics


main = hspec spec
spec = do
  spec_driftRateToPeriod
  spec_driftRateToSMA
  spec_meanAngularMotion

spec_driftRateToPeriod = describe "driftRateToPeriod" $ do

  it "is the inverse of periodToDriftRate"
    (property $ \(d::AngularVelocity Double) ->
      d ~== periodToDriftRate (driftRateToPeriod d))

spec_driftRateToSMA = describe "smaToDriftRate" $ do

  it "is the inverse of driftRateToSMA"
    (property $ \(NonNegativeD (a::Length Double)) ->
      a ~== driftRateToSMA (smaToDriftRate a))

spec_meanAngularMotion = describe "meanAngularMotion" $ do

  it "is the inverse of semiMajorAxis"
      a ~== semiMajorAxis (meanAngularMotion a))
    (property $ \(NonNegativeD (a::Length Double)) ->
