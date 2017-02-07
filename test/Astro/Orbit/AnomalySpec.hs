{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Astro.Orbit.AnomalySpec where

import Test.Hspec
import Test.QuickCheck (property, (==>))
import Data.AEq

import TestUtil
import TestInstances

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude

import Astro.Orbit.Types
import Astro.Orbit.Anomaly
import Astro.Util.Cyclic (fractionalPart, adjustZeroOne)


main = hspec spec
spec = do
  spec_anomalyComparison
  spec_anomalyConversion


-- ----------------------------------------------------------
spec_anomalyComparison = describe "Anomaly comparisons" $ do

  it "-pi and pi should be equal"
    (Anom (negate pi) == Anom pi)

  it "-pi and pi should be approximately equal"
    (Anom (negate pi::Angle Double) ~== Anom pi)

  it "0 and tau should be equal"
    (Anom _0 == Anom (tau))

  it "0 and tau should be approximately equal"
    (Anom _0 ~== Anom (tau::Angle Double))

  it "x and x+tau should be equal."
    (property $ \t -> Anom t ~== Anom (t + tau::Angle Double))


-- ----------------------------------------------------------
spec_anomalyConversion = describe "Anomaly conversions" $ do

  -- Arguably should use adjustZeroOne instead of fractionalPart in the
  -- below tests, but these seem to work well with negative eccentricity
  -- too!
  --
  -- Could use the Arbitrary instance for Eccentricity, but would have
  -- to discard eccentricity >= 1.

  it "Two ways of computing eccentric anomaly from true anomaly are identical."
    (property $ \e' t -> let e = fractionalPart e'
      in eccAnomaly1 e t ~== eccAnomaly2 e t)

  it "Converting TA to EA is consistent with other method."
    (property $ \e' t -> let e = Ecc $ fractionalPart e' :: Eccentricity Double
      in ta2ea e t ~== Anom (eccAnomaly1 (ecc e) (anom t)))

  it "Converting TA to EA and back should not change it."
    (property $ \e' t -> let e = Ecc $ fractionalPart e' :: Eccentricity Double
      in (ea2ta e . ta2ea e) t ~== t)

  it "At perigee TA and EA should be equally 0."
    (property $ \e' -> let e = Ecc $ fractionalPart e'
      in ta2ea e ta0 == ea0 && ea2ta e ea0 == ta0)

  it "At apogee TA and EA should be equally pi."
    (property $ \e' -> let e = Ecc $ adjustZeroOne e' :: Eccentricity Double
      in ta2ea e (Anom pi) ~== Anom pi && ea2ta e (Anom pi) == Anom pi)

  it "For circular orbit TA and EA should be equal."
    (property $ \a -> ta2ea e0 (Anom a) ~== Anom a && ea2ta e0 (Anom a) ~== Anom a)

  it "Converting EA to MA and back should not change it."
    (property $ \e' ea -> let e = Ecc $ fractionalPart e' :: Eccentricity Double
      in (ma2ea e . ea2ma e) ea ~== ea)

  it "Converting EA to MA and back should not change it. (Failure case)" $ do
    let e  = Ecc $ fractionalPart (2.4344584473354116 *~ one) :: Eccentricity Double
        ea = Anom $ (-3.3609126772415534) *~ radian
      in (ma2ea e . ea2ma e) ea ~== ea

  it "At perigee EA and MA should be equally 0."
    (property $ \e' -> let e = Ecc $ fractionalPart e'
      in ea2ma e ea0 == ma0 && ma2ea e ma0 == ea0)

  it "At apogee EA and MA should be equally pi."
    (property $ \e' -> let e = Ecc $ fractionalPart e' :: Eccentricity Double
      in ea2ma e (Anom pi) == Anom pi && ma2ea e (Anom pi) == Anom pi)

  it "For circular orbit EA and MA should be equal."
    (property $ \a -> ea2ma e0 (Anom a) ~== Anom a && ma2ea e0 (Anom a) ~== Anom a)

  where
    e0 = Ecc _0 :: Eccentricity Double
    ta0 = Anom _0 :: Anomaly True Double
    ea0 = Anom _0 :: Anomaly Eccentric  Double
    ma0 = Anom _0 :: Anomaly Mean Double


-- | Compute eccentric anomaly using atan.
eccAnomaly1 :: Dimensionless Double -> Angle Double -> Angle Double
eccAnomaly1 e t = _2 * atan (sqrt ((_1 - e) / (_1 + e)) * tan (t / _2))
-- | Compute eccentric anomaly using atan2.
eccAnomaly2 :: Dimensionless Double -> Angle Double -> Angle Double
eccAnomaly2 e t = atan2 (sqrt (_1 - e ^ pos2) * sin t) (e + cos t)
