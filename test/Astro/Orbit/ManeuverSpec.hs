{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Astro.Orbit.ManeuverSpec where

import Test.Hspec
import Test.QuickCheck (property, (==>))
import Data.AEq

import TestUtil
import TestInstances

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude

import Astro.Time
import Astro.Time.At

import Astro.Orbit.COE
import Astro.Orbit.Conversion
import Astro.Orbit.Types
import Astro.Orbit.Maneuver
import Astro.Trajectory
import Astro.Trajectory.EphemTrajectory


main = hspec spec
spec = do
  spec_zeroManeuver
  spec_randomManeuver


-- ----------------------------------------------------------
spec_zeroManeuver = describe "Zero maneuver " $ do

  it "does not affect the trajectory"
    ( ephemeris (applyManeuver testTrajectory (zeroMan`At`mjd' 1)) (map mjd' [0..])
    ~== ephemeris testTrajectory (map mjd' [0..])
    )

  where
    zeroMan = ImpulsiveRTN (0*~mps) (0*~mps) (0*~mps)


spec_randomManeuver = describe "Random maneuver at time t" $ do

  it "does not affect trajectories that start after t"
    (property $ \m
      -> ephemeris (applyManeuver testTrajectory (m`At`mjd' (-1))) (map mjd' [0..])
      == ephemeris testTrajectory (map mjd' [0..])
    )

  it "does not affect trajectories that end before t"
    (property $ \m
      -> ephemeris (applyManeuver testTrajectory (m`At`mjd' 11)) (map mjd' [0..])
      == ephemeris testTrajectory (map mjd' [0..])
    )

  it "does not affect data prior to time t"
    (property $ \m
      -> absoluteDV m > _0
      ==> ephemeris (applyManeuver testTrajectory (m`At`mjd' 5)) (map mjd' [0..4])
      == ephemeris testTrajectory (map mjd' [0..4])
    )

  it "affects data after time t" $
    property $ \m
      -> absoluteDV m > _0
      ==> ephemeris (applyManeuver testTrajectory (m`At`mjd' 5)) (map mjd' [6])
      /= ephemeris testTrajectory (map mjd' [6])

absoluteDV :: Floating a => Maneuver a -> Velocity a
absoluteDV ImpulsiveRTN {..} = sqrt (dvr^pos2 + dvt^pos2 + dvn^pos2)

-- ----------------------------------------
mps = meter / second

testCOE0 :: COE Mean Double
testCOE0 = COE
  { mu = mu_Earth
  , slr = 10000 *~ kilo meter
  , ecc = 0 *~ one
  , inc = 0 *~ degree
  , aop = 0 *~ degree
  , raan = 0 *~ degree
  , anomaly = Anom $ 0 *~ degree
  }

testCOE1 :: COE Mean Double
testCOE1 = COE
  { mu = mu_Earth
  , slr = 24000 *~ kilo meter
  , ecc = 0.01 *~ one
  , inc = 15 *~ degree
  , aop = (-105) *~ degree -- 255 *~ degree
  , raan = 35 *~ degree
  , anomaly = Anom $ 10 *~ degree
  }

testTrajectory = ET [coe2meoe testCOE0`At`mjd' 0, coe2meoe testCOE1`At`mjd' 10]
mu_Earth = 398600.4418 *~ (kilo meter ^ pos3 / second ^ pos2)
