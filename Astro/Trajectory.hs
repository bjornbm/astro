{-# LANGUAGE MultiParamTypeClasses #-}

module Astro.Trajectory
  ( Trajectory, startTime, endTime, ephemeris, ephemeris'
  , Datum
  ) where

import Astro.Time
import Astro.Time.At
import Astro.Orbit.MEOE
import Astro.Orbit.Types
import Numeric.Units.Dimensional.Prelude

type Datum t a = At t a (MEOE Mean a)

-- | A @Trajectory@ is an abstraction for a position-velocity
-- path through space in a particular time window.
class (Fractional a, Ord a) => Trajectory x t a
  where
    -- | The earliest epoch at which this trajectory is valid.
    startTime  :: x t a -> E t a
    -- | The last epoch at which this trajectory is valid.
    endTime    :: x t a -> E t a
    -- | Produce an ephemeris with the given epochs.
    -- The list of epochs must be increasing. If two identical
    -- epochs are encountered behaviour is unspecified (one or
    -- two data may be produced for that epoch).
    ephemeris  :: x t a -> [E t a] -> [Datum t a]
    -- | @ephemeris' t0 t1 dt@ produces an ephemeris starting
    -- at @t0@ with data every @dt@ until @t1@. A datum at @t1@
    -- is produced only if @(t1 - t0) / dt@ is an integer.
    ephemeris' :: x t a -> E t a -> E t a -> Time a -> [Datum t a]
    ephemeris' x t0 t1 dt = ephemeris x ts
      where
        ts = takeWhile (<=t1) $ iterate (`addTime` dt) t0
