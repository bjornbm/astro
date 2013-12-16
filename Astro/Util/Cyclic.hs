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


-- Adjusting values with respect to a reference
-- --------------------------------------------

-- | Removes the integral part of a value so that it ends up in the
-- interval [0,1). This differs from (snd . properFraction) for
-- negative values:
--   @(snd . properFraction)   1.2@  =  0.2
--   @adjustZeroOne            1.2@  =  0.2
--   @(snd . properFraction) (-1.2)@ = -0.2
--   @adjustZeroOne          (-1.2)@ =  0.8
adjustZeroOne :: RealFrac a => Dimensionless a -> Dimensionless a
adjustZeroOne = fmap (\x -> x Prelude.- fromIntegral (floor x))

-- | A somewhat misleadingly named alias for 'adjustZeroOne'
-- (@/= snd . properFraction@).
fractionalPart :: RealFrac a => Dimensionless a -> Dimensionless a
fractionalPart = adjustZeroOne

-- | @adjustAbove1 min x@ adjusts x by an integer so that it lies in the
-- interval [min,min+1).
adjust1Above :: RealFrac a
             => Dimensionless a -> Dimensionless a -> Dimensionless a
adjust1Above min x = adjustZeroOne (x - min) + min

-- | @adjustAbove cycle min x@ adjusts x by an integer number or cycles
-- so that it lies in the interval [min,min+cycle).
adjustAbove :: (RealFrac a, Div d d DOne, Mul DOne d d)
            => Quantity d a -> Quantity d a -> Quantity d a -> Quantity d a
adjustAbove cycle min x = adjust1Above (min/cycle) (x/cycle) * cycle

-- | @adjustAbout1 center x@ adjusts x by an integer so that it lies in the
-- interval [center-1/2,center+1/2).
adjust1About :: RealFrac a
             => Dimensionless a -> Dimensionless a -> Dimensionless a
adjust1About center = adjust1Above (center - _1/_2)

-- | @adjustAbout center cycle x@ adjusts x by an integer number or cycles
-- so that it lies in the interval [min-cycle/2,min+cycle/2).
adjustAbout :: (RealFrac a, Div d d DOne, Mul DOne d d, Div d DOne d)
            => Quantity d a -> Quantity d a -> Quantity d a -> Quantity d a
adjustAbout center cycle x = adjust1About (center/cycle) (x/cycle) * cycle


-- Adjusting angles
-- ----------------

-- | @adjustAngle center a@ adjusts the angle @a@ to
-- be within ±π of @center@.
adjustAngle :: RealFloat a => Angle a -> Angle a -> Angle a
adjustAngle center = adjustAbout center tau

-- | Adjusts an angle to the range [-pi,pi).
plusMinusPi :: RealFloat a => Angle a -> Angle a
plusMinusPi = adjustAngle _0

-- | Adjusts an angle to the range [0,2pi).
zeroTwoPi :: RealFloat a => Angle a -> Angle a
zeroTwoPi = adjustAngle pi


-- Consistency metric for cyclic values
-- ------------------------------------

-- | Assume that y(x) is cyclic in meaning (but not in value, as
-- for e.g. angles) with a periodicity @period@ and cycle length @cycle@.
-- Then @cyclesOff (x,y) period cycle@ compute the approximate (closest)
-- integral number of cycles that @y@ differs from what one would expect
-- given @x@ and an assumption that y = cycle * x / period.
cyclesOff :: (RealFrac a, Div d d DOne, Div dy dy DOne, Mul DOne dy dy)
       => (Quantity d a, Quantity dy a)
       -> Quantity d a -> Quantity dy a -> Dimensionless a
cyclesOff (x,y) period cycle = cyclesOff1 (x/period, y/cycle)

-- | Same as 'cyclesOff' but assumes that both the period and the cycle
-- of y(x) is one.
cyclesOff1 :: RealFrac a => (Dimensionless a, Dimensionless a) -> Dimensionless a
cyclesOff1 (x, y) = fmap (fromIntegral . round) (x - y)


-- Adjusting for relative total rotation
-- -------------------------------------

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

-- | Assume that y(x) is cyclic in meaning (but not in value, as
-- for e.g. angles) where the meaning has a cycle (in y) of 1 with
-- a period (in x) of 1. Then @adjustCyclic1 (x0,y0) (x1,y1)@
-- returns a new @y1@ adjusted so that @y1 - y0@ is roughly the same
-- as @x1 - x0@.
--
-- Property of returned y1:
--   | (y1 - y0) - (x1 - x0) | < 0.5
--
-- (This is a "normalized" version of 'adjustCyclic' for cycle and
-- period of 1.)
adjustCyclic1 :: (RealFrac a, Ord a)
              => (Dimensionless a, Dimensionless a)
              -> (Dimensionless a, Dimensionless a)
              -> Dimensionless a
adjustCyclic1 (x0,y0) (x1,y1) = y1 + cyclesOff1 (x1-x0, y1-y0)

-- | Adjust assuming x0 = 0 and y0 = 0
adjustCyclic0 :: (RealFrac a, Div d d DOne, Div dy dy DOne, Mul DOne dy dy)
              => (Quantity d a, Quantity dy a)
              -> Quantity d a -> Quantity dy a -> Quantity dy a
adjustCyclic0 (x1,y1) period cycle = y1 + cyclesOff (x1,y1) period cycle * cycle

-- | Assume that y(t) is cyclic in meaning (but not in value, as
-- for e.g. angles). Then @adjustCyclic (y0`At`t0) (y1`At`t1) period cycle@
-- returns a new @y1@ adjusted so that the difference @y1 - y0@
-- corresponds roughly to the difference @t1 - t0@.
-- (See also the more general adjustCyclic.)
adjustCyclicT :: (RealFrac a, Div dy dy DOne, Mul DOne dy dy)
              => At t a (Quantity dy a) -> At t a (Quantity dy a)
              -> Time a -> Quantity dy a -> Quantity dy a
adjustCyclicT (y0`At`t0) (y1`At`t1) = adjustCyclic (f t0 ,y0) (f t1, y1)
  where f t = diffEpoch t (mjd' 0)
