module Astro.Trajectory.CroppedSpec where

import Test.Hspec
--import Test.QuickCheck (property, (==>))

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

main = hspec spec
spec = do
  spec_uncropped
  spec_croppedStartTime
  spec_croppedEndTime
  spec_badValidity
  spec_croppedEphemeris
  spec_croppedEphemeris'

-- Don't think using QuickCheck is warranted in this case as
-- the test MEOEs are very random. We implement our own 'property'
-- instead which feed the test MEOEs.
property f = f testM1 testM2

-- ----------------------------------------------------------------------

-- TODO move this spec elsewhere!
spec_uncropped = describe "Uncropped trajectory" $ do

  it "does not change startTime when not cropping"
    (let t = ET [undefined `At` t2, undefined `At` t4] in
      startTime (crop' Nothing Nothing t) `shouldBe` t2)

  it "does not change endTime when not cropping"
    (let t = ET [undefined `At` t2, undefined `At` t4] in
      endTime (crop' Nothing Nothing t) `shouldBe` t4)

  it "does not change ephemeris' when not cropping"
    (property $ \m m' -> let t = ET [m `At` t2, m' `At` t3] in
      ephemeris' (crop' Nothing Nothing t) t0 t5 dt
      == ephemeris' t t0 t5 dt)

  it "returns no ephemeris' beyond lower validity"
    (property $ \m m' -> let t = ET [m `At` t2, m' `At` t3] in
      ephemeris' (crop' Nothing Nothing t) t0 t1 dt == [])

  it "returns no ephemeris' beyond upper validity"
    (property $ \m m' -> let t = ET [m `At` t2, m' `At` t3] in
      ephemeris' (crop' Nothing Nothing t) t4 t5 dt == [])

  it "does not change ephemeris when not cropping"
    (property $ \m m' ->
      let t = ET [m `At` t2, m' `At` t3]; ts = takeWhile (<= t5) $ iterate (`addTime` dt) t0 in
      -- let t = ET [m `At` t2, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
      ephemeris (crop' Nothing Nothing t) ts == ephemeris t ts)

  it "returns no ephemeris beyond lower validity"
    (property $ \m m' ->
      let t = ET [m `At` t2, m' `At` t3]; ts = takeWhile (<= t1) $ iterate (`addTime` dt) t0 in
      -- let t = ET [m `At` t2, m' `At` t3]; ts = [t0, t0 `addTime` dt..t1] in
      ephemeris (crop' Nothing Nothing t) ts == [])

  it "returns no ephemeris beyond upper validity"
    (property $ \m m' ->
      let t = ET [m `At` t2, m' `At` t3]; ts = takeWhile (<= t5) $ iterate (`addTime` dt) t4 in
      -- let t = ET [m `At` t2, m' `At` t3]; ts = [t4, t4 `addTime` dt..t5] in
      ephemeris (crop' Nothing Nothing t) ts == [])

-- ----------------------------------------------------------------------

spec_croppedStartTime = describe "Cropped trajectory startTime" $ do

  it "does not change when cropping before validity"
    (let t = ET [undefined `At` t2, undefined `At` t4] in
      startTime (cropStart t1 t) `shouldBe` t2)

  it "does not change when cropping endTime"
    (let t = ET [undefined `At` t2, undefined `At` t4] in
      startTime (cropEnd t3 t) `shouldBe` t2)

  it "changes when cropping startTime"
    (let t = ET [undefined `At` t2, undefined `At` t4] in
      startTime (cropStart t3 t) `shouldBe` t3)

-- ----------------------------------------------------------------------

spec_croppedEndTime = describe "Cropped trajectory endTime" $ do

  it "does not change when cropping after validity"
    (let t = ET [undefined `At` t2, undefined `At` t4] in
      endTime (crop t1 t5 t) `shouldBe` t4)

  it "does not change when cropping startTime"
    (let t = ET [undefined `At` t2, undefined `At` t4] in
      endTime (cropStart t3 t) `shouldBe` t4)

  it "changes when cropping endTime"
    (let t = ET [undefined `At` t2, undefined `At` t4] in
      endTime (cropEnd t3 t) `shouldBe` t3)

spec_badValidity = describe "Trajectory with cropping disjunct from validity" $ do

  it "doesn't generate ephemeris when cropped early"
    (let t = crop t1 t2 $ ET [undefined `At` t3, undefined `At` t4] in
      ephemeris t [startTime t, endTime t] `shouldBe` [])

  it "doesn't generate ephemeris when cropped late"
    (let t = crop t3 t4 $ ET [undefined `At` t1, undefined `At` t2] in
      ephemeris t [startTime t, endTime t] `shouldBe` [])

  it "doesn't generate ephemeris' when cropped early"
    (let t = crop t1 t2 $ ET [undefined `At` t3, undefined `At` t4] in
      ephemeris' t (startTime t) (endTime t) dt `shouldBe` [])

  it "doesn't generate ephemeris' when cropped late"
    (let t = crop t3 t4 $ ET [undefined `At` t1, undefined `At` t2] in
      ephemeris' t (startTime t) (endTime t) dt `shouldBe` [])

-- ----------------------------------------------------------------------

spec_croppedEphemeris' = describe "Cropped trajectory (ephemeris')" $ do

  it "does not change when cropping beyond validity"
    (property $ \m m' -> let t = ET [m `At` t2, m' `At` t3] in
      ephemeris' t t0 t5 dt
      == ephemeris' (crop t1 t4 t) t0 t5 dt)

  it "with cropped startTime is not equal to uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t3] in
      (ephemeris' (crop t2 t4 t) t0 t5 dt
      /= ephemeris' t t0 t5 dt))

  it "with cropped startTime is suffix of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t3] in
      (ephemeris' (crop t2 t4 t) t0 t5 dt
      `isSuffixOf` ephemeris' t t0 t5 dt))

  it "with cropped endTime is not equal to of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t3] in
      (ephemeris' (cropEnd t2 t) t0 t5 dt
      /= ephemeris' t t0 t5 dt))

  it "with cropped endTime is prefix of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t3] in
      (ephemeris' (cropEnd t2 t) t0 t5 dt
      `isPrefixOf` ephemeris' t t0 t5 dt))

  it "with cropped start and end is not prefix of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t4] in
      not (ephemeris' (crop t2 t3 t) t0 t5 dt
      `isPrefixOf` ephemeris' t t0 t5 dt))

  it "with cropped start and end is not suffix of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t4] in
      not (ephemeris' (crop t2 t3 t) t0 t5 dt
      `isSuffixOf` ephemeris' t t0 t5 dt))

  it "with cropped start and end is infix of uncropped trajectory"
    (property $ \m m' -> let t = ET [m `At` t1, m' `At` t4] in
      (ephemeris' (crop t2 t3 t) t0 t5 dt
      `isInfixOf` ephemeris' t t0 t5 dt))

  it "returns no ephemeris' beyond lower validity"
    (property $ \m m' -> let t = ET [m `At` t0, m' `At` t5] in
      ephemeris' (crop t2 t3 t) t0 t1 dt == [])

  it "returns no ephemeris' beyond upper validity"
    (let t = ET [undefined `At` t0, undefined `At` t5] in
      ephemeris' (crop t2 t3 t) t4 t5 dt `shouldBe` [])

-- ----------------------------------------------------------------------

spec_croppedEphemeris = describe "Cropped trajectory (ephemeris)" $ do

  it "does not change when cropping beyond validity"
    (property $ \m m' ->
      let t = ET [m `At` t2, m' `At` t3]; ts = takeWhile (<= t5) $ iterate (`addTime` dt) t0 in
      -- let t = ET [m `At` t2, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
       ephemeris t ts == ephemeris (crop t1 t4 t) ts)

  it "with cropped startTime is not equal to uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t3]; ts = takeWhile (<= t5) $ iterate (`addTime` dt) t0 in
      -- let t = ET [m `At` t1, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
      ephemeris (crop t2 t4 t) ts /= ephemeris t ts)

  it "with cropped startTime is suffix of uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t3]; ts = takeWhile (<= t5) $ iterate (`addTime` dt) t0 in
      -- let t = ET [m `At` t1, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
      (ephemeris (crop t2 t4 t) ts
      `isSuffixOf` ephemeris t ts))

  it "with cropped endTime is not equal to of uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t3]; ts = takeWhile (<= t5) $ iterate (`addTime` dt) t0 in
      -- let t = ET [m `At` t1, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
      ephemeris (cropEnd t2 t) ts /= ephemeris t ts)

  it "with cropped endTime is prefix of uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t3]; ts = takeWhile (<= t5) $ iterate (`addTime` dt) t0 in
      -- let t = ET [m `At` t1, m' `At` t3]; ts = [t0, t0 `addTime` dt..t5] in
      (ephemeris (cropEnd t2 t) ts
      `isPrefixOf` ephemeris t ts))

  it "with cropped start and end is not prefix of uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t4]; ts = takeWhile (<= t5) $ iterate (`addTime` dt) t0 in
      -- let t = ET [m `At` t1, m' `At` t4]; ts = [t0, t0 `addTime` dt..t5] in
      not (ephemeris (crop t2 t3 t) ts
      `isPrefixOf` ephemeris t ts))

  it "with cropped start and end is not suffix of uncropped trajectory"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t4]; ts = takeWhile (<= t5) $ iterate (`addTime` dt) t0 in
      -- let t = ET [m `At` t1, m' `At` t4]; ts = [t0, t0 `addTime` dt..t5] in
      not (ephemeris (crop t2 t3 t) ts
      `isSuffixOf` ephemeris t ts))

  it "with cropped start and end is infix of uncropped trajectory (ephemeris')"
    (property $ \m m' ->
      let t = ET [m `At` t1, m' `At` t4]; ts = takeWhile (<= t5) $ iterate (`addTime` dt) t0 in
      -- let t = ET [m `At` t1, m' `At` t4]; ts = [t0, t0 `addTime` dt..t5] in
      (ephemeris (crop t2 t3 t) ts
      `isInfixOf` ephemeris t ts))

  it "returns no ephemeris beyond lower validity"
    (let t = ET [undefined `At` t0, undefined `At` t5]
         ts = takeWhile (<= t1) $ iterate (`addTime` dt) t0
        --  ts = [t0, t0 `addTime` dt..t1]
      in ephemeris (crop t2 t3 t) ts `shouldBe` [])

  it "returns no ephemeris beyond upper validity"
    (let t = ET [undefined `At` t0, undefined `At` t5]
         ts = takeWhile (<= t5) $ iterate (`addTime` dt) t4
        --  ts = [t4, t4 `addTime` dt..t5]
      in ephemeris (crop t2 t3 t) ts `shouldBe` [])

-- ----------------------------------------------------------------------

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
