{-
[1] http://books.google.com/books?id=PJLlWzMBKjkC&pg=RA1-PA197
-}
module Astro.TestTime where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro
import Astro.DefaultData
import Astro.Time
import Astro.Time.Convert
import qualified Astro.Time.Barycentric.Kaplan2005 as Kaplan2005
import qualified Astro.Time.Barycentric.AsA2009    as AsA2009
import qualified Astro.Time.Barycentric.TT         as TT
--import Astro.Time.Barycentric.TT
import Test.QuickCheck
import Data.Time hiding (utc)
import Data.Time.Clock.TAI

-- Preliminaries
-- =============

onceCheck = quickCheckWith stdArgs { maxSuccess = 1 }
conv t = runAstro (convert t) defaultAstroData

-- | Comparison allowing for inaccuracy.
cmpE :: (Fractional a, Ord a) => Time a -> E t a -> E t a -> Bool
cmpE accuracy t t' = abs (diffEpoch t t') < accuracy

-- Accuracies.
tdbError = 20 *~ micro second  -- About 10 us according to Kaplan, but for Vallado's cases the difference is greater...
dblError =  1 *~ nano second
noError  =  0 *~ second




-- Specific epochs
-- ===============

-- J200 epoch.
prop_J2000_jd    = j2000 ==  jd  2451545 TT
prop_J2000_mjd   = j2000 == mjd  51544.5 TT
prop_J2000_clock = j2000 == clock 2000 1 1 12 0 0 TT
prop_J2000_TAI   = conv j2000 == clock 2000 1 1 11 59 27.816 TAI 
-- prop_J2000_UTC   = convertUTC () j2000 == clock 2000 1 1 11 58 55.816 UTC

-- Convergence epoch
-- -----------------
ttConverges  = clock 1977 01 01 00 00 32.184 TT  == conv (clock 1977 1 1 0 0 0 TAI)
tcgConverges = clock 1977 01 01 00 00 32.184 TCG == conv (clock 1977 1 1 0 0 0 TAI)
-- The accuracy of TDB and TCB are limited by the TDB conversions.
tcbConverges = cmpE tdbError (clock 1977 01 01 00 00 32.184     TCB) (conv $ clock 1977 1 1 0 0 0 TAI) 
tdbConverges = cmpE tdbError (clock 1977 01 01 00 00 32.1839345 TDB) (conv $ clock 1977 1 1 0 0 0 TAI) 
-- This first test isn't adversely affected by conversion to Double while the second is.
prop_TDBTCB_conv  = clock 1977 01 01 00 00 32.184 TCB == conv (clock 1977 01 01 00 00 32.1839345 TDB)
prop_TDBTCB_conv' = cmpE dblError (conv $ clock 1977 01 01 00 00 32.184 TCB) (clock 1977 01 01 00 00 32.1839345 TDB)


-- Conversion tests
-- ================
-- Check conversions back and forth are consistent. Test around both
-- JD 0 and MJD 0 to make sure we aren't favoring algorithms that are
-- only good around certain dates.
prop_TAI, prop_TT, prop_TCG, prop_TDB, prop_TCB, prop_TCB' :: Double -> Bool
-- These should be exact.
prop_TAI a = let t =  jd a TAI in t == conv (conv t ::  E TAI Double)  -- ^ Conversion back and forth to TAI.
          && let t = mjd a TAI in t == conv (conv t ::  E TAI Double)  -- ^ Conversion back and forth to TAI.
prop_TT  a = let t =  jd a TT  in t == conv (conv t ::  E TAI Double)  -- ^ Conversion back and forth to TAI.
          && let t = mjd a TT  in t == conv (conv t ::  E TAI Double)  -- ^ Conversion back and forth to TAI.
-- In the remaining double precision errors come into play.
prop_TCG a = let t =  jd a TCG in cmpE dblError t (conv (conv t ::  E TAI Double))  -- ^ Conversion back and forth to TAI.
          && let t = mjd a TCG in cmpE dblError t (conv (conv t ::  E TAI Double))  -- ^ Conversion back and forth to TAI.
-- Accuracy of TDB|TCB <-> TAI is limited by the accuracy of TDB <-> TAI. 
-- However, the reverse conversion appears to cancels the error nicely.
prop_TDB a = let t =  jd a TDB in cmpE dblError t (conv (conv t ::  E TAI Double))  -- ^ Conversion back and forth to TAI.
          && let t = mjd a TDB in cmpE dblError t (conv (conv t ::  E TAI Double))  -- ^ Conversion back and forth to TAI.



prop_TCB a = let t =  jd a TCB in cmpE dblError t (conv (conv t ::  E TAI Double))  -- ^ Conversion back and forth to TAI.
{-
*** Failed! Falsifiable (after 25 tests):  
26.303659304105857
-}


          && let t = mjd a TCB in cmpE dblError t (conv (conv t ::  E TAI Double))  -- ^ Conversion back and forth to TAI.
-- The accuracy of TCB <-> TDB conversions should be good. (This test is mostly redundant given prop_TCB).
prop_TCB' a = let t =  jd a TCB in cmpE dblError t (conv (conv t :: E TDB Double))  -- ^ Conversion back and forth to TDB.
           && let t = mjd a TCB in cmpE dblError t (conv (conv t :: E TDB Double))  -- ^ Conversion back and forth to TDB.



-- Tests from literature
-- =====================

-- utc 1990 05 14 10 43 00.000 from [1].
valladoExample = tai == conv tt
              && cmpE tdbError (tai) (conv tdb)
              && cmpE tdbError (tt ) (conv tdb)
  where 
    tai = clock 1990 05 14 16 43 25.000 TAI
    tt  = clock 1990 05 14 16 43 57.184 TT
    tdb = clock 1990 05 14 16 43 57.18527 TDB

-- utc 2004 04 06 07 51 28.386009 from [2].
valladoExample2 = tai == conv tt && conv tai == tt
               && cmpE dblError tai (conv tcg)    -- Test conversion from TCG.
               && cmpE dblError (conv tai) tcg  -- Test conversion to TCG.
               && cmpE tdbError tai (conv tdb)    -- Test conversion from TDB.
               && cmpE tdbError (conv tai) tdb  -- Test conversion to TDB.
               -- && cmpE dblError (tdbToTCB tdb) tcb         -- Test conversion to TCB.
               -- && cmpE tdbError (fromTAI $ toTAI tai) tcb  -- Test conversion to TCB.
  --where
tai = clock 2004 04 06 7 52 00.386009 TAI
tt  = clock 2004 04 06 7 52 32.570009 TT
tcg = clock 2004 04 06 7 52 33.1695861742 TCG
tdb = clock 2004 04 06 7 52 32.5716651154 TDB
tcb = clock 2004 04 06 7 52 45.9109901113 TCB  -- Incorrect in example, no offset.


-- Test TT/TDB conversions
-- -------------------------------
-- Test AsA2009 conversion modules.
-- Should only be testing between 1980 (MJD 44239) and 2050 (MJD 70172).
prop_bary_AsA2009_1 a = let t = mjd a TDB :: E TDB Double in 
  cmpE (30*~micro second) (AsA2009.tdbToTT t) (Kaplan2005.tdbToTT t)
prop_bary_AsA2009_2 a = let t = mjd a TT :: E TT Double in 
  cmpE (30*~micro second) (AsA2009.ttToTDB t) (Kaplan2005.ttToTDB t)

-- Verify accuracy of TDB = TT.
prop_bary_TT_1 a = let t = mjd a TDB :: E TDB Double in 
  cmpE (1.7*~milli second) (TT.tdbToTT t) (Kaplan2005.tdbToTT t)
prop_bary_TT_2 a = let t = mjd a TT :: E TT Double in 
  cmpE (1.7*~milli second) (TT.ttToTDB t) (Kaplan2005.ttToTDB t)

-- Driver
-- ======

main = do
  -- Start with the faster-running tests.
  onceCheck prop_J2000_jd
  onceCheck prop_J2000_mjd
  onceCheck prop_J2000_clock
  onceCheck prop_J2000_TAI
  onceCheck prop_TDBTCB_conv
  onceCheck prop_TDBTCB_conv'
  onceCheck ttConverges
  onceCheck tcgConverges
  onceCheck tcbConverges
  onceCheck tdbConverges
  onceCheck valladoExample
  onceCheck valladoExample2
  quickCheck prop_TAI
  quickCheck prop_TT
  quickCheck prop_TCG
  quickCheck prop_TDB
  quickCheck prop_TCB
  quickCheck prop_TCB'
  quickCheck prop_bary_AsA2009_1
  quickCheck prop_bary_AsA2009_2
  quickCheck prop_bary_TT_1
  quickCheck prop_bary_TT_2
 

