{-# LANGUAGE MultiParamTypeClasses #-}

{- |
   Copyright  : Copyright (C) 2008 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : experimental
   Portability: GHC only?

An implementation of TDB conversions based on formula (2.6) of
[Kaplan2005]. The formula reportedly has a maximum error in the
conversions of about 10 microseconds between the years 1600 and
2200.

This module exports no data types or functions, it only provides
additional 'Astro.Time.Convert' instances.
-}
module Astro.Time.Barycentric.Kaplan2005 (tdbToTT, ttToTDB) where

import Astro.Time
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


-- | The difference between the TDB and TT time scales as a function of
-- TT epoch.
tdbMinusTT :: Floating a => E TT a -> Time a
tdbMinusTT tt = 0.001657*~second * sin ( 628.3076 *~rpc * t + 6.2401 *~radian)
              + 0.000022*~second * sin ( 575.3385 *~rpc * t + 4.2970 *~radian)
              + 0.000014*~second * sin (1256.6152 *~rpc * t + 6.1969 *~radian)
              + 0.000005*~second * sin ( 606.9777 *~rpc * t + 4.0212 *~radian)
              + 0.000005*~second * sin (  52.9691 *~rpc * t + 0.4444 *~radian)
              + 0.000002*~second * sin (  21.3299 *~rpc * t + 5.5431 *~radian)
              + 0.000010*~(second/century) * t * sin ( 628.3076 *~rpc * t + 4.2490 *~radian)
  where
    rpc = radian / century
    t   = diffEpoch tt j2000

{-
We incorrectly substitute TDB for TT in 'ttMinusTDB' below but the error
introduced by this approximation is less than a picosecond (far less than
the 10 microsecond accuracy inherent in the formula in the first place).
-}
-- | The difference between the TDB and TT time scales as a function of
-- TT epoch.
ttMinusTDB :: Floating a => E TDB a -> Time a
ttMinusTDB = negate . tdbMinusTT . coerceE


-- | Convert a TT epoch into a TDB epoch.
ttToTDB :: Floating a => E TT a -> E TDB a
ttToTDB tt = coerceE $ addTime tt (tdbMinusTT tt)

-- | Convert a TDB epoch into a TT epoch.
tdbToTT :: Floating a => E TDB a -> E TT a
tdbToTT tdb = coerceE $ addTime tdb (ttMinusTDB tdb)
