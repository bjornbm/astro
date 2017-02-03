{-# LANGUAGE ScopedTypeVariables #-}

module Astro.Coords.TNRSpec where

import Test.Hspec
import Test.QuickCheck
import Data.AEq

import TestUtil
import TestInstances

import Astrodynamics (greenwichRA)
import Astro.Coords
import Astro.Coords.ECR
import Astro.Coords.TNR
import Astro.Coords.PosVel
import Astro.Time -- (UT1, E, addTime)
import Astro.Time.At
import Astro.Util (perfectGEO, perfectGEO')
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.PosVel (Car)
import qualified Prelude


main = hspec spec
spec = do
  spec_trivialSV
  spec_trivialRetroSV
  spec_eciToECR
  spec_ecrToECIPV
  spec_eciToECRPV


-- ----------------------------------------------------------
spec_trivialSV = describe "A trivial state vector" $ do

  it "gives a correct orbital (TNR) frame" $
    orbitalFrame pv == _Y |: _Z |:. _X

  it "gives a correct attiude (RPY) frame" $
    attitudeCoordSys pv ~== _Y |: vNegate _Z |:. vNegate _X

  where
    r = scaleVec ((1::Double) *~ meter) _X :: Car DLength Double
    v = scaleVec (1 *~ (meter / second)) _Y
    pv = C' r v

spec_trivialRetroSV = describe "A trivial retrograde state vector" $ do

  it "gives a correct orbital (TNR) frame" $
    orbitalFrame pv == vNegate _Y |: vNegate _Z |:. _X

  it "gives a correct attiude (RPY) frame" $
    attitudeCoordSys pv ~== vNegate _Y |: _Z |:. vNegate _X

  where
    r = scaleVec ((1::Double) *~ meter) _X
    v = scaleVec (1 *~ (meter / second)) _Y
    pv = C' r (vNegate v)

-- ----------------------------------------------------------
spec_eciToECR = describe "eciToECR" $ do

  it "is the inverse of ecrToECI"
    (property $ \(p::Coord ECI D) t ->
      (ecrToECI t . eciToECR t) p ~== p)


-- ----------------------------------------------------------
spec_ecrToECIPV = describe "ecrToECIPV" $ do

  it "is identical to ecrToECI for the position"
    (property $ \(pv::PosVel ECR D) t ->
      --Comparison of cartesian coords fails due to numerics.
      (s . pos . ecrToECIPV t) pv ~== (s . ecrToECI t . pos) pv)

  it "is the inverse of eciToECRPV"
    (property $ \(pv::PosVel ECR D) t ->
      (eciToECRPV t . ecrToECIPV t) pv ~== pv)


-- ----------------------------------------------------------
spec_eciToECRPV = describe "eciToECRPV" $ do

  it "is identical to eciToECR for the position"
    (property $ \(pv::PosVel ECI D) t ->
      --Comparison of cartesian coords fails due to numerics.
      (s . pos . eciToECRPV t) pv ~== (s . eciToECR t . pos) pv)

  it "is the inverse of ecrToECIPV"
    (property $ \(pv::PosVel ECI D) t ->
      (ecrToECIPV t . eciToECRPV t) pv ~== pv)

-- {-
p = (-0.5653349232735853::D) *~ meter <: (-5.133004275272132) *~meter <:. (-7.22445929855348) *~ meter
t = clock' 1858 11 24 20 10 15.151878680541
pv = C' p (_0 <: _0 <:. _0)
-- -}
