{-# LANGUAGE ScopedTypeVariables #-}

module Astro.Time.AtSpec where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck (property, (==>))

import Numeric.Units.Dimensional.Prelude
import qualified Prelude

import Astro.Time
import Astro.Time.At

import TestInstances

main = hspec spec
spec = do
  spec_allIn
  spec_zipAtWith

-- Utility

-- | Checks that all elements in the first list are contained in the
-- second list.
allIn :: (Eq a, Ord a) => [a] -> [a] -> Bool
allIn xs ys = allIn' (sort xs) (sort ys)

-- | Checks that all elements in the first list are contained in the
-- second list. The lists must be sorted.
allIn' :: Eq a => [a] -> [a] -> Bool
allIn' []     _      = True
allIn' _      []     = False
allIn' (a:as) (b:bs) = if a == b then allIn' as     bs
                                 else allIn' (a:as) bs

spec_allIn = describe "allIn" $ do

  it "is True when it should be"
    $  allIn [1,2,3] [1..100]
    && allIn [2,4]   [1..100]
    && allIn [2,4]   [1..4]

  it "is False when it should be"
    $  not (allIn [1,2,3] [2..100])
    && not (allIn [3,4] [1..3])

  it "works with a single element" $ property $
    \(x::Double) xs -> allIn [x] xs == elem x xs

-- Actual tests.

spec_zipAtWith = describe "zipAtWith" $ do

  it "drops values when time doesn't match" $ property $
    \(xs::[At UT1 Double Int]) ys ->
      let  ts = map epoch $ zipAtWith (Prelude.+) (sort xs) (sort ys)
      in ts `allIn` map epoch xs && ts `allIn` map epoch ys

  it "works for some handrolled cases" $
    let ats = map (At 0 . mjd') :: [Double] -> [At UT1 Double Int]
    in    zipAtWith const (ats [1..10])   (ats [5..20])   == ats [5..10]
       && zipAtWith const (ats [5..20])   (ats [1..10])   == ats [5..10]
       && zipAtWith const (ats [5..20])   (ats [1..10])   == ats [5..10]
       && zipAtWith const (ats [4])       (ats [1..10])   == ats [4]
       && zipAtWith const (ats [1,3..10]) (ats [2,4..10]) == []
       && zipAtWith const (ats [1..11])   (ats [2,4..28]) == ats [2,4..10]
         -- Beware of broken Enum for Double!
