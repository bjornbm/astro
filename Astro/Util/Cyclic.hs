{-# LANGUAGE FlexibleContexts #-}

-- | This module contains various function for working with functions
-- that are is cyclic in meaning (but not in value). An obvious example
-- are angles, where in many cases there is no value in distinguishing
-- between x and x + n2π.

module Astro.Util.Cyclic where

import qualified Prelude
import Numeric.Units.Dimensional.Prelude

import Astro.Time
import Astro.Time.At


-- | Assume that y(t) is cyclic in meaning (but not in value, as
-- for e.g. angles). Then @adjustCyclic (t0,y0) (t1,y1) period cycle@
-- returns a new @y1@ adjusted so that the difference @y1 - y0@
-- corresponds roughly to the difference @t1 - t0@.
-- (See also the more general adjustCyclic.)
adjustCyclicT :: (RealFrac a, Div dy dy DOne, Mul DOne dy dy)
              => At t a (Quantity dy a) -> At t a (Quantity dy a)
              -> Time a -> Quantity dy a -> Quantity dy a
adjustCyclicT (y0`At`t0) (y1`At`t1) = adjustCyclic (f t0 ,y0) (f t1, y1)
  where f t = diffEpoch t (mjd' 0)

-- | Assume that y(x) is cyclic in meaning (but not in value, as
-- for e.g. angles). Then @adjustCyclic (x0,y0) (x1,y1) period cycle@
-- returns a new @y1@ adjusted so that the difference @y1 - y0@
-- corresponds roughly to the difference @x1 - x0@.
-- (See also adjustCyclic1.)
adjustCyclic :: (RealFrac a, Div d d DOne, Div dy dy DOne, Mul DOne dy dy)
            => (Quantity d a, Quantity dy a) -> (Quantity d a, Quantity dy a)
            -> Quantity d a -> Quantity dy a -> Quantity dy a
adjustCyclic (x0,y0) (x1,y1) period cycle =
  y1 + cyclesOff (x1-x0, y1-y0) period cycle * cycle
  -- Could be defined as:
  --   adjustCyclic1 (x0/period,y0/cycle) (x1/period,y1/cycle) * cycle
  -- but that has worse numerical properties!

-- | Adjust assuming x0 = 0 and y0 = 0
adjustCyclic0 :: (RealFrac a, Div d d DOne, Div dy dy DOne, Mul DOne dy dy)
              => (Quantity d a, Quantity dy a)
              -> Quantity d a -> Quantity dy a -> Quantity dy a
adjustCyclic0 (x1,y1) period cycle = y1 + cyclesOff (x1,y1) period cycle * cycle

-- | Assume that y(x) is cyclic in meaning (but not in value, as
-- for e.g. angles) where the meaning has a cycle (in y) of 1 with
-- a period (in x) of 1. Then @adjustCyclic1 (x0,y0) (x1,y1)@
-- returns a new @y1@ adjusted so that @y1 - y0@ is roughly the same
-- as @x1 - x0@.
--
-- Property of returned y1:
--   | (y1 - y0) - (x1 - x0) | < 0.5
--
-- (This is a "normalized" version of adjustCyclic for cycle and
-- period of 1.)
adjustCyclic1 :: (RealFrac a, Ord a)
              => (Dimensionless a, Dimensionless a)
              -> (Dimensionless a, Dimensionless a)
              -> Dimensionless a
adjustCyclic1 (x0,y0) (x1,y1) = y1 + cyclesOff1 (x1-x0, y1-y0)



-- | Assume that y(x) is cyclic in meaning (but not in value, as
-- for e.g. angles) with a periodicity @period@ and cycle length @cycle@.
-- Then @cyclesOff (x,y) period cycle@ compute the approximate (closest)
-- integral number of cycles that @y@ differs from what one would expect
-- given @x@ and an assumption that y = cycle * x / period.
cyclesOff :: (RealFrac a, Div d d DOne, Div dy dy DOne, Mul DOne dy dy)
       => (Quantity d a, Quantity dy a)
       -> Quantity d a -> Quantity dy a -> Dimensionless a
cyclesOff (x,y) period cycle = cyclesOff1 (x/period, y/cycle)

-- | Same as 'cyclesOff' but assumes that bot the period and the cycle
-- of y(x) is one.
cyclesOff1 :: RealFrac a => (Dimensionless a, Dimensionless a) -> Dimensionless a
cyclesOff1 (x, y) = fmap (fromIntegral . round) (x - y)
