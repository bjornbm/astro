{-# LANGUAGE ScopedTypeVariables #-}

module Astro.Time.SiderealSpec where

import Test.Hspec
import Test.QuickCheck (property, (==>), NonZero(..))
import Data.AEq

import TestUtil
import TestInstances

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.AD (diff)
import Numeric.Units.Dimensional.Cyclic
import Numeric.Units.Dimensional.NonSI (revolution)
import qualified Prelude

import Astro.Util
import Astro.Time
import Astro.Time.Sidereal


main = hspec spec
spec = do
  spec_era
  spec_phi
  spec_gmst


-- ----------------------------------------------------------
spec_era = describe "Earth Rotation Angle (era)" $ do

  it "is consistent with AsA2009 on 2009-01-01" $
    (zeroTwoPi $ era $ clock' 2009 1 1 0 0 0 :: Angle D)
    ~== sexagesimalAngle 100 39 39.6557

  it "is consistent with AsA2009 on 2010-01-01" $
    (zeroTwoPi $ era $ clock' 2010 1 1 0 0 0 :: Angle D)
    ~== sexagesimalAngle 100 24 34.2022


spec_phi = describe "Earth's adopted mean angular velocity (phi)" $ do

  it "is the time derivative of the Earth rotation angle"
    (property $ \(t :: Time D) -> phi == diff (era . addTime j2000') t)


spec_gmst = describe "GMST" $ do

  it "is consistent with AsA2009 on 2009-01-01" $
    (zeroTwoPi $ gmst' $ clock' 2009 1 1 0 0 0 :: Angle D)
    ~== timeAngle 6 43 6.3205

  it "is consistent with AsA2009 on 2010-01-01" $
    (zeroTwoPi $ gmst' $ clock' 2010 1 1 0 0 0 :: Angle D)
    ~== timeAngle 6 42 9.0298
