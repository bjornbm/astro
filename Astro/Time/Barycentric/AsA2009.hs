{-# LANGUAGE MultiParamTypeClasses #-}

{- |
   Copyright  : Copyright (C) 2008 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : experimental
   Portability: GHC only?

An implementation of TDB conversions based on the formula on page
B7 of the Astronomical Almanac for the year 2009. The formula
reportedly has a maximum error in the conversions of about 30
microseconds over the period 1980 to 2050.
-}
module Astro.Time.Barycentric.AsA2009 (tdbToTT, ttToTDB) where

import Astro.Time
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


-- | The difference between the TDB and TT time scales as a function of
-- TT epoch.
tdbMinusTT :: Floating a => E TT -> Time a
tdbMinusTT tt = 0.001657*~second * sin g + 0.000022*~second * sin deltaL
  where
    t   = diffEpoch tt j2000  
    -- The mean anomaly of the Earth in its orbit around the Sun.
    g = 357.53*~degree + 0.98560028*~(degree/day) * t
    -- The difference in the mean ecliptic longitudes of the Sun and Jupiter.
    deltaL = 246.11*~degree + 0.90251792*~(degree/day) * t

{-
We incorrectly substitute TDB for TT in 'ttMinusTDB' below but the error
introduced by this approximation is far less than the 30 microsecond accuracy
inherent in the formula in the first place.
-}
-- | The difference between the TDB and TT time scales as a function of
-- TT epoch.
ttMinusTDB :: Floating a => E TDB -> Time a
ttMinusTDB (E t) = negate $ tdbMinusTT (E t)


-- | Convert a TT epoch into a TDB epoch.
ttToTDB :: E TT -> E TDB
ttToTDB tt@(E t) = E $ addTime t (tdbMinusTT tt)

-- | Convert a TDB epoch into a TT epoch.
tdbToTT :: E TDB -> E TT
tdbToTT tdb@(E t) = E $ addTime t (ttMinusTDB tdb)

