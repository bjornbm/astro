-- | This module contains various function for working with functions
-- that are is cyclic in meaning (but not in value). An obvious example
-- are angles, where in many cases there is no value in distinguishing
-- between x and x + n2π.

module Astro.Util.Cyclic where

import qualified Prelude
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.Cyclic ( adjustCyclic )

import Astro.Time
import Astro.Time.At


-- | Assume that y(t) is cyclic in meaning (but not in value, as
-- for e.g. angles). Then @adjustCyclic (y0`At`t0) (y1`At`t1) period cycle@
-- returns a new @y1@ adjusted so that the difference @y1 - y0@
-- corresponds roughly to the difference @t1 - t0@.
-- (See also the more general adjustCyclic.)
adjustCyclicT :: RealFrac a
              => At t a (Quantity dy a) -> At t a (Quantity dy a)
              -> Time a -> Quantity dy a -> Quantity dy a
adjustCyclicT (y0`At`t0) (y1`At`t1) = adjustCyclic (f t0 ,y0) (f t1, y1)
  where f t = diffEpoch t (mjd' 0)
