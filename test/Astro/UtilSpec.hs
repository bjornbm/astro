{-# LANGUAGE ScopedTypeVariables #-}

module Astro.UtilSpec where

import Test.Hspec
import Test.QuickCheck (property, (==>))
import Data.AEq

import TestUtil
import TestInstances

import Numeric.Units.Dimensional.Prelude
import qualified Prelude

import Astro.Util
import Astro.Util.Cyclic
import Astro.Coords
import Astro.Coords.PosVel
import Astro.Place
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.PosVel


main = hspec spec
spec = do
  spec_perfectGEO
  spec_perfectGEO'


spec_perfectGEO = describe "Coordinates generated by perfectGEO" $ do

  it "have the desired radius" $ property $
    \(l::GeoLongitude Double) -> (radius . s . perfectGEO) l ~== r_GEO

  it "have the desired longitude" $ property $
    \(l::GeoLongitude Double) -> (rightAscension . s . perfectGEO) l ~==~ geoLongitude l

  it "have the desired zenith angle in ECR" $ property $
    \(l::GeoLongitude Double) -> (zenith . s . perfectGEO) l == tau / _4

  it "do not have an out-of-plane component (cartesian)" $ property $
    \(l::GeoLongitude Double) -> (vElemAt n2 . c . perfectGEO) l == _0


spec_perfectGEO' = describe "PosVels generated by perfectGEO'" $ do

  it "have zero velocity in ECR frame" $ property $
    \(l::GeoLongitude Double) -> (norm . cvel . perfectGEO') l == _0

  it "have zero velocity in ECR frame (spherical)" $ property $
    \(l::GeoLongitude Double) -> (magnitude . svel . perfectGEO') l == _0

  where
    magnitude (SVel m' _ _) = m'
