{-# LANGUAGE ScopedTypeVariables #-}

module Astro.Coords.ECRSpec where

import Test.Hspec
import Test.QuickCheck
import Data.AEq

import TestUtil
import TestInstances

import Astrodynamics (greenwichRA)
import Astro.Coords
import Astro.Coords.ECR
import Astro.Coords.PosVel
import Astro.Time -- (UT1, E, addTime)
import Astro.Time.At
import Astro.Util (perfectGEO, perfectGEO')
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude


main = hspec spec
spec = do
  spec_ecrToECI


-- ----------------------------------------------------------
spec_ecrToECI = describe "ecrToECI" $ do
  it "doesn't add out-of plane components to a perfect GEO"
    (property $ \(l::Angle Double) t ->
      (vElemAt pos2 $ c $ ecrToECI t $ perfectGEO l) == _0)
