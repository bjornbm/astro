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
import Astro.Place
import Astro.Time -- (UT1, E, addTime)
import Astro.Time.At
import Astro.Util (perfectGEO, perfectGEO')
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.PosVel (radius)
import qualified Prelude


main = hspec spec
spec = do
  spec_ecrToECI
  spec_eciToECR
  spec_ecrToECIPV
  spec_eciToECRPV


-- ----------------------------------------------------------
spec_ecrToECI = describe "ecrToECI" $ do

  it "doesn't add out-of plane components to a perfect GEO" $ property $
    \(l::GeoLongitude D) t -> (vElemAt n2 . c . ecrToECI t . perfectGEO) l == _0

  it "is the inverse of eciToECR" $ property $
    \(p::Coord ECR D) t -> (eciToECR t . ecrToECI t) p ~== p

  it "does not change the out-of-plane component" $ property $
    \(p::Coord ECR D) t -> vElemAt n2 (c $ ecrToECI t p) == vElemAt n2 (c p)

  it "does not change the radius" $ property $
    \(p::Coord ECR D) t -> radius (s $ ecrToECI t p) ~== radius (s p)


-- ----------------------------------------------------------
spec_eciToECR = describe "eciToECR" $ do

  it "is the inverse of ecrToECI" $ property $
    \(p::Coord ECI D) t -> (ecrToECI t . eciToECR t) p ~== p


-- ----------------------------------------------------------
spec_ecrToECIPV = describe "ecrToECIPV" $ do

  it "is identical to ecrToECI for the position" $ property $
    --Comparison of cartesian coords fails due to numerics.
    \(pv::PosVel ECR D) t -> (s . pos . ecrToECIPV t) pv ~== (s . ecrToECI t . pos) pv

  it "is the inverse of eciToECRPV" $ property $
    \(pv::PosVel ECR D) t -> (eciToECRPV t . ecrToECIPV t) pv ~== pv


-- ----------------------------------------------------------
spec_eciToECRPV = describe "eciToECRPV" $ do

  it "is identical to eciToECR for the position" $ property $
    --Comparison of cartesian coords fails due to numerics.
    \(pv::PosVel ECI D) t -> (s . pos . eciToECRPV t) pv ~== (s . eciToECR t . pos) pv

  it "is the inverse of ecrToECIPV" $ property $
    \(pv::PosVel ECI D) t -> (ecrToECIPV t . eciToECRPV t) pv ~== pv

-- {-
p = (-0.5653349232735853::D) *~ meter <: (-5.133004275272132) *~meter <:. (-7.22445929855348) *~ meter
t = clock' 1858 11 24 20 10 15.151878680541
pv = C' p (_0 <: _0 <:. _0)
-- -}
