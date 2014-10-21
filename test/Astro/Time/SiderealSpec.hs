{-# LANGUAGE ScopedTypeVariables #-}

module Astro.Time.SiderealSpec where

import Test.Hspec
import Test.QuickCheck (property, (==>), NonZero(..))
import Data.AEq

import TestUtil
import TestInstances

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.AD (diff)
import Numeric.Units.Dimensional.NonSI (revolution)
import qualified Prelude

import Astro.Util.Cyclic
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
    ~== 100 *~ degree + 39 *~ arcminute + 39.6557 *~ arcsecond

  it "is consistent with AsA2009 on 2010-01-01" $
    (zeroTwoPi $ era $ clock' 2010 1 1 0 0 0 :: Angle D)
    ~== 100 *~ degree + 24 *~ arcminute + 34.2022 *~ arcsecond


spec_phi = describe "Earth's adopted mean angular velocity (phi)" $ do

  it "is the time derivative of the Earth rotation angle"
    (property $ \(t :: Time D) -> phi == diff (era . addTime j2000') t)


spec_gmst = describe "GMST" $ do

  it "is consistent with AsA2009 on 2009-01-01" $
    (zeroTwoPi $ gmst' $ clock' 2009 1 1 0 0 0 :: Angle D)
    ~== 6 *~ h + 43 *~ m + 6.3205 *~ s

  it "is consistent with AsA2009 on 2010-01-01" $
    (zeroTwoPi $ gmst' $ clock' 2010 1 1 0 0 0 :: Angle D)
    ~== 6 *~ h + 42 *~ m + 9.0298 *~ s

  where
    h = prefix (1 Prelude./ 24) revolution
    m = prefix (1 Prelude./ 60) h
    s = prefix (1 Prelude./ 60) m
