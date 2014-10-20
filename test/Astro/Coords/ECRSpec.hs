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
  spec_eciToECR


-- ----------------------------------------------------------
spec_ecrToECI = describe "ecrToECI" $ do

  it "doesn't add out-of plane components to a perfect GEO"
    (property $ \(l::Angle D) t ->
      (vElemAt pos2 $ c $ ecrToECI t $ perfectGEO l) == _0)

  it "is the inverse of eciToECR"
    (property $ \(p::Coord ECR D) t ->
      (eciToECR t . ecrToECI t) p ~== p)

  it "does not change the out-of-plane component"
    (property $ \(p::Coord ECR D) t ->
      vElemAt pos2 (c $ ecrToECI t p) == vElemAt pos2 (c p))

  it "does not change the radius"
    (property $ \(p::Coord ECR D) t ->
      vElemAt zero (s $ecrToECI t p) ~== vElemAt zero (s p))


-- ----------------------------------------------------------
spec_eciToECR = describe "eciToECR" $ do

  it "is the inverse of ecrToECI"
    (property $ \(p::Coord ECI D) t ->
      (ecrToECI t . eciToECR t) p ~== p)

