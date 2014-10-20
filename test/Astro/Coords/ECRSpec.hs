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
  spec_ecrToECISV
  spec_eciToECRSV


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


-- ----------------------------------------------------------
spec_ecrToECISV = describe "ecrToECISV" $ do

  it "is identical to ecrToECI for the position"
    (property $ \(pv::PosVel ECR D) t ->
      --Comparison of cartesian coords fails due to numerics.
      (s . pos . ecrToECISV t) pv ~== (s . ecrToECI t . pos) pv)

  it "is the inverse of eciToECRSV"
    (property $ \(pv::PosVel ECR D) t ->
      (eciToECRSV t . ecrToECISV t) pv ~== pv)


-- ----------------------------------------------------------
spec_eciToECRSV = describe "eciToECRSV" $ do

  it "is identical to eciToECR for the position"
    (property $ \(pv::PosVel ECI D) t ->
      --Comparison of cartesian coords fails due to numerics.
      (s . pos . eciToECRSV t) pv ~== (s . eciToECR t . pos) pv)

  it "is the inverse of ecrToECISV"
    (property $ \(pv::PosVel ECI D) t ->
      (ecrToECISV t . eciToECRSV t) pv ~== pv)

-- {-
p = (-0.5653349232735853::D) *~ meter <: (-5.133004275272132) *~meter <:. (-7.22445929855348) *~ meter
t = clock' 1858 11 24 20 10 15.151878680541
pv = C' p (_0 <: _0 <:. _0)
-- -}
