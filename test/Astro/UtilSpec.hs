{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Astro.UtilSpec where

import Test.Hspec
import Test.QuickCheck (property, (==>))
import Data.AEq

import TestUtil
import TestInstances

import Numeric.Units.Dimensional.Prelude
import qualified Prelude

import Astro.Util



main = hspec spec
spec = do
  return ()

-- All specs have been moved to Astro.Util.CyclicSpec!
