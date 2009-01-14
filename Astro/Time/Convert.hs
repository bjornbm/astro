{-
This module provides (monadic) conversion between all time scales.
We use overlapping instances to reduce the number of instance
declarations necessary. One could instead use brute force and provide
explicit 'Convert' instances for all combinations of time scales.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Astro.Time.Convert where

import Astro
import Astro.Time
import Control.Monad.Reader
import Data.Time.Clock.TAI (LeapSecondTable)
import Data.Fixed
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


-- ConvertTT
class ConvertTT t where 
  toTT   :: E t  -> Astro a (E TT)
  fromTT :: E TT -> Astro a (E t)

-- All time scales should be able to convert to/from TT.
instance ConvertTT TCG where 
  toTT   = return . tcgToTT
  fromTT = return .  ttToTCG
instance ConvertTT TDB where 
  toTT   = eval (tdbToTT .time)
  fromTT = eval ( ttToTDB.time)
instance ConvertTT TCB where 
  toTT   = toTT . tcbToTDB
  fromTT t = fromTT t >>= return . tdbToTCB
instance ConvertTT TAI where 
  toTT   = return . taiToTT
  fromTT = return .  ttToTAI
instance ConvertTT UT1 where 
  toTT   t = eval (ut1ToTAI.time) t >>= toTT
  fromTT t = fromTT t >>= eval (taiToUT1.time)

-- General epoch conversion.
class Convert t t' where convert :: E t -> Astro a (E t')

-- Trivial 'id' version. Perhaps this shouldn't be provided??
instance Convert a  a  where convert = return  -- Trivial.
instance Convert TT TT where convert = return  -- Trivial, needed for specificity.

-- Conversion from TT.
instance (ConvertTT t) => Convert TT t where convert = fromTT
instance (ConvertTT t) => Convert t TT where convert = toTT

-- Generic Convert instance, via TT.
instance (ConvertTT t, ConvertTT t') => Convert t t'
  where convert t = toTT t >>= fromTT

-- Specific conversion instances for cases where going via
-- TT would be prohibitively expensive or lossy (in accuracy).
instance Convert TDB TCB where convert = return . tdbToTCB
instance Convert TCB TDB where convert = return . tcbToTDB

