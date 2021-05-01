{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Astro.Orbit.MEOESpec where

import Test.Hspec
import Test.QuickCheck (property, (==>))
import Data.AEq

import TestUtil
import TestInstances

import Numeric.Units.Dimensional.Cyclic ((~==~))
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude

import Astro.Orbit.MEOE
import Astro.Orbit.Types


main = hspec spec
spec = do
  spec_inclination
  spec_aop
  spec_aol
  spec_anomaly
  spec_rate
  spec_impulsive

type MT = MEOE True Double
type MM = MEOE Mean Double

spec_inclination = describe "MEOE inclination" $ do
  it "equals 2c in Eagle"
    (property $ \(m@MEOE{..} :: MT) -> let h2 = h ^ pos2; k2 = k ^ pos2 in
      inclination m ~== atan2 (_2 * sqrt (h2 + k2)) (_1 - h2 - k2))

spec_aop = describe "MEOE argumentOfperiapsis" $ do
  it "equals 2d (middle, but using atan2) in Eagle"
    (property $ \(m@MEOE{..} :: MT) ->
      argumentOfPeriapsis m ~==~ atan2 g f - atan2 k h)
  {- FAILS: When h = k = 0
  it "equals 2d (RHS) in Eagle"
    (property $ \(m@MEOE {..} :: MT) ->
      argumentOfPeriapsis m ~==~ atan2 (g * h - f * k) (f * h - g * k))
  -}
  it "equals ecliptic longitude - RAAN - anomaly"
    (property $ \(m@MEOE{..} :: MT) ->
      argumentOfPeriapsis m ~==~ long longitude - raan m - anom (anomaly m))

spec_anomaly = describe "MEOE anomaly" $ do
  it "equals 2f in Eagle"
    (property $ \(m@MEOE{..} :: MT) ->
      anom (anomaly m) ~==~ long longitude - atan2 g f)
  it "equals ecliptic longitude - RAAN - AoP"
    (property $ \(m :: MT) ->
      anom (anomaly m) ~==~ long (longitude m) - (raan m + argumentOfPeriapsis m))

spec_aol = describe "MEOE argumentOflatitude" $ do
  it "equals 2g in Eagle"
    (property $ \(m :: MT) ->
      argumentOfLatitude m ~==~ argumentOfPeriapsis m + anom (anomaly m))

spec_rate = describe "MEOE longitude rate" $ do

  it "is equal to mean longitude rate when eccentricity is zero"
    (property $ \(m' :: MT) -> let m = m' { f = _0, g = _0 } in
      longitudeRate m ~== meanLongitudeRate m)

  it "is not equal to mean longitude rate when eccentricity /= zero"
    (property $ \(m :: MT) -> eccentricity2 m > _0 ==>
      longitudeRate m /= meanLongitudeRate m)

spec_impulsive = describe "Impulsive perturbations" $ do

  it "of zero magnitude do not change the MEOE"
    (property $ \(m :: MT) ->
      impulsivePerturbation m (ImpulsiveRTN _0 _0 _0) == m)

  it "of non-zero magnitude change the MEOE"
    (property $ \(m::MT) dvr dvt dvn ->
      dvr /= _0 || dvt /= _0 || dvn /= _0 ==>
      not (impulsivePerturbation m (ImpulsiveRTN dvr dvt dvn) ~== m))

{-
  it "do not change the current position"
    (property $ \(m::MT) dvr dvt dvn ->
      position (impulsivePerturbation m dvr dvt dvn) ~== position m
    ) where position = undefined
-- -}

-- {-
testM :: MEOE True Double
testM = MEOE
  { mu = mu_Earth
  , p = 24000 *~ kilo meter
  , f = 0.01 *~ one
  , g = 0.01 *~ one
  , h = 0.02 *~ one
  , k = (-0.01) *~ one
  , longitude = Long (123 *~ degree)
  }

mps = meter / second

-- Convenience and utility functions.

-- | From Wikipedia.
mu_Earth = 398600.4418 *~ (kilo meter ^ pos3 / second ^ pos2)
-- -}
