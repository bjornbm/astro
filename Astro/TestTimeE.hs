{-
[1] http://books.google.com/books?id=PJLlWzMBKjkC&pg=RA1-PA197&lpg=RA1-PA197&dq=tdb+tt&source=web&ots=V0jT1Cnfsl&sig=LNVZxWoDhWWoe7_V1Sn07kZgcvM&hl=en&sa=X&oi=book_result&resnum=1&ct=result#PRA1-PA197,M1
-}


import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro.TimeE
import Test.QuickCheck
import Data.Time hiding (utc)
import Data.Time.Clock.TAI

-- Accuracies.
tdbError = 20 *~ micro second  -- About 10 us according to Kaplan, but for Vallado's cases the difference is greater...
dblError =  1 *~ nano second

-- Comparison allowing for inaccuracy.
cmpE :: (DiffEpoch t, Fractional a, Ord a) => Time a -> t -> t -> Bool
cmpE accuracy t t' = abs (diffEpoch t t') < accuracy

-- J200 epoch.
prop_J2000_jd    = j2000 == jd 2451545 TT
prop_J2000_clock = j2000 == clock 2000 1 1 12 0 0 TT

-- Check conversions back and forth are consistent. Test around both
-- JD 0 and MJD 0 to make sure we aren't favoring algorithms that are
-- only good around certain dates.
prop_TAI, prop_TT, prop_TCG, prop_TDB, prop_TCB, prop_TCB' :: Double -> Bool
-- These should be exact.
prop_TAI a = let t =  jd a TAI in t == convert (convert t :: E TAI)  -- ^ Conversion back and forth to TAI.
          && let t = mjd a TAI in t == convert (convert t :: E TAI)  -- ^ Conversion back and forth to TAI.
prop_TT  a = let t =  jd a TT  in t == convert (convert t :: E TAI)  -- ^ Conversion back and forth to TAI.
          && let t = mjd a TT  in t == convert (convert t :: E TAI)  -- ^ Conversion back and forth to TAI.
-- In the remaining double precision errors come into play.
prop_TCG a = let t =  jd a TCG in cmpE dblError t (convert (convert t :: E TAI))  -- ^ Conversion back and forth to TAI.
          && let t = mjd a TCG in cmpE dblError t (convert (convert t :: E TAI))  -- ^ Conversion back and forth to TAI.
-- Accuracy of TDB|TCB <-> TAI is limited by the accuracy of TDB <-> TAI. 
-- However, the reverse conversion appears to cancels the error nicely.
prop_TDB a = let t =  jd a TDB in cmpE dblError t (convert (convert t :: E TAI))  -- ^ Conversion back and forth to TAI.
          && let t = mjd a TDB in cmpE dblError t (convert (convert t :: E TAI))  -- ^ Conversion back and forth to TAI.
prop_TCB a = let t =  jd a TCB in cmpE dblError t (convert (convert t :: E TAI))  -- ^ Conversion back and forth to TAI.
          && let t = mjd a TCB in cmpE dblError t (convert (convert t :: E TAI))  -- ^ Conversion back and forth to TAI.
-- The accuracy of TCB <-> TDB conversions should be good. (This test is mostly redundant given prop_TCB).
prop_TCB' a = let t =  jd a TCB in cmpE dblError t (convert (convert t :: E TDB))  -- ^ Conversion back and forth to TDB.
           && let t = mjd a TCB in cmpE dblError t (convert (convert t :: E TDB))  -- ^ Conversion back and forth to TDB.

-- Test convergence epochs.
ttConverges  = clock 1977 01 01 00 00 32.184 TT  == convert (clock 1977 1 1 0 0 0 TAI)
tcgConverges = clock 1977 01 01 00 00 32.184 TCG == convert (clock 1977 1 1 0 0 0 TAI)
-- The accuracy of TDB and TCB are limited by the TDB conversions.
tcbConverges = cmpE tdbError (clock 1977 01 01 00 00 32.184     TCB) (convert $ clock 1977 1 1 0 0 0 TAI) 
tdbConverges = cmpE tdbError (clock 1977 01 01 00 00 32.1839345 TDB) (convert $ clock 1977 1 1 0 0 0 TAI) 

-- utc 1990 05 14 10 43 00.000 from [1].
valladoExample = tai == convert tt
              && cmpE tdbError (tai) (convert tdb)
              && cmpE tdbError (tt ) (convert tdb)
  where 
    tai = clock 1990 05 14 16 43 25.000 TAI
    tt  = clock 1990 05 14 16 43 57.184 TT
    tdb = clock 1990 05 14 16 43 57.18527 TDB

-- utc 2004 04 06 07 51 28.386009 from [2].
valladoExample2 = tai == convert tt && convert tai == tt
               && cmpE dblError tai (convert tcg)    -- Test conversion from TCG.
               && cmpE dblError (convert tai) tcg  -- Test conversion to TCG.
               && cmpE tdbError tai (convert tdb)    -- Test conversion from TDB.
               && cmpE tdbError (convert tai) tdb  -- Test conversion to TDB.
               -- && cmpE dblError (tdbToTCB tdb) tcb         -- Test conversion to TCB.
               -- && cmpE tdbError (fromTAI $ toTAI tai) tcb  -- Test conversion to TCB.
  --where
tai = clock 2004 04 06 7 52 00.386009 TAI
tt  = clock 2004 04 06 7 52 32.570009 TT
tcg = clock 2004 04 06 7 52 33.1695861742 TCG
tdb = clock 2004 04 06 7 52 32.5716651154 TDB
tcb = clock 2004 04 06 7 52 45.9109901113 TCB  -- Incorrect in example, no offset.


onceCheck = check (defaultConfig {configMaxTest = 1})
main = do
  -- Start with the faster-running tests.
  onceCheck prop_J2000_jd
  onceCheck prop_J2000_clock
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

