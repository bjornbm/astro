module Astro.Trajectory.Cropped.Tests where

import Test.Hspec
--import Test.QuickCheck (property, (==>))

import TestUtil
import TestInstances

import Astro.Trajectory
import Astro.Trajectory.Cropped
import Astro.Trajectory.EphemTrajectory

import qualified Prelude
import Numeric.Units.Dimensional.Prelude
import Astro.Time
import Astro.Time.At

import Data.AEq
import Data.List
import Astro.Orbit.MEOE
import Astro.Orbit.Types

main = hspec specs
specs = do
  spec_croppedEphemeris
  spec_croppedEphemeris'

-- Don't think using QuickCheck is warranted in this case as
-- the test MEOEs are very random. We implement our own 'property'
-- instead which feed the test MEOEs.
property f = f testM1 testM2

spec_croppedEphemeris' = describe "Cropped trajectory (ephemeris')" $ do

  it "does not change when not cropping"
    (property $ \m m' -> let t = ET [m `At` t2, m' `At` t3] in
      ephemeris' t t0 t5 dt
      == ephemeris' (cropTrajectory Nothing Nothing t) t0 t5 dt)

  it "does not change when cropping beyond validity"
    (property $ \m m' -> let t = ET [m `At` t2, m' `At` t3] in
      ephemeris' t t0 t5 dt
      == ephemeris' (cropTrajectory (Just t1) (Just t4) t) t0 t5 dt)

  it "with cropped startTime is not equal to uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t3] in
      (ephemeris' (cropTrajectory (Just t2) (Just t4) t) t0 t5 dt
      /= ephemeris' t t0 t5 dt))

  it "with cropped startTime is suffix of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t3] in
      (ephemeris' (cropTrajectory (Just t2) (Just t4) t) t0 t5 dt
      `isSuffixOf` ephemeris' t t0 t5 dt))

  it "with cropped endTime is not equal to of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t3] in
      (ephemeris' (cropTrajectory Nothing (Just t2) t) t0 t5 dt
      /= ephemeris' t t0 t5 dt))

  it "with cropped endTime is prefix of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t3] in
      (ephemeris' (cropTrajectory Nothing (Just t2) t) t0 t5 dt
      `isPrefixOf` ephemeris' t t0 t5 dt))

  it "with cropped start and end is not prefix of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t4] in
      not (ephemeris' (cropTrajectory (Just t2) (Just t3) t) t0 t5 dt
      `isPrefixOf` ephemeris' t t0 t5 dt))

  it "with cropped start and end is not suffix of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t4] in
      not (ephemeris' (cropTrajectory (Just t2) (Just t3) t) t0 t5 dt
      `isSuffixOf` ephemeris' t t0 t5 dt))

  it "with cropped start and end is infix of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t4] in
      (ephemeris' (cropTrajectory (Just t2) (Just t3) t) t0 t5 dt
      `isInfixOf` ephemeris' t t0 t5 dt))


spec_croppedEphemeris = describe "Cropped trajectory (ephemeris)" $ do

  it "does not change when not cropping"
    (property $ \m m' ->
      let t = ET [m `At` t2, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
      ephemeris t ts == ephemeris (cropTrajectory Nothing Nothing t) ts)

  it "does not change when cropping beyond validity"
    (property $ \m m' ->
      let t = ET [m `At` t2, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
       ephemeris t ts == ephemeris (cropTrajectory (Just t1) (Just t4) t) ts)

  it "with cropped startTime is not equal to uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
      ephemeris (cropTrajectory (Just t2) (Just t4) t) ts /= ephemeris t ts)

  it "with cropped startTime is suffix of uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
      (ephemeris (cropTrajectory (Just t2) (Just t4) t) ts
      `isSuffixOf` ephemeris t ts))

  it "with cropped endTime is not equal to of uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
      ephemeris (cropTrajectory Nothing (Just t2) t) ts /= ephemeris t ts)

  it "with cropped endTime is prefix of uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
      (ephemeris (cropTrajectory Nothing (Just t2) t) ts
      `isPrefixOf` ephemeris t ts))

  it "with cropped start and end is not prefix of uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t4]; ts = [t0, t0 `addTime` dt..t5] in
      not (ephemeris (cropTrajectory (Just t2) (Just t3) t) ts
      `isPrefixOf` ephemeris t ts))

  it "with cropped start and end is not suffix of uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t4]; ts = [t0, t0 `addTime` dt..t5] in
      not (ephemeris (cropTrajectory (Just t2) (Just t3) t) ts
      `isSuffixOf` ephemeris t ts))

  it "with cropped start and end is infix of uncropped trajectory (ephemeris')"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t4]; ts = [t0, t0 `addTime` dt..t5] in
      (ephemeris (cropTrajectory (Just t2) (Just t3) t) ts
      `isInfixOf` ephemeris t ts))

t0 = mjd 0.0 UT1
t1 = mjd 1 UT1
t2 = mjd 2 UT1
t3 = mjd 3 UT1
t4 = mjd 4 UT1
t5 = mjd 5 UT1
dt  = 1 *~ hour :: Time Double

-- Test data, essentially randomized.
testM1 = MEOE { mu = 5.5017577174388266e9 *~ (meter^pos3/second^pos2)
              , p = 0.7865893064609859 *~meter, f = 0.6398323179864169*~one
              , g = 0.0996399428802211 *~one, h = (-0.7813921023837359)*~one
              , k = 0.7396666870016642 *~one
              , longitude = Long { long = 0.811762241416502*~one }
              }
testM2 = MEOE { mu = 4.5017577174388266e9 *~ (meter^pos3/second^pos2)
              , p = 0.6865893064609859 *~meter, f = 0.2398323179864169*~one
              , g = 0.1996399428802211 *~one, h = (-0.0813921023837359)*~one
              , k = 0.1396666870016642 *~one
              , longitude = Long { long = 2.811762241416502*~one }
              }
