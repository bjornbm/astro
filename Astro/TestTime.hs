{-
[1] http://books.google.com/books?id=PJLlWzMBKjkC&pg=RA1-PA197&lpg=RA1-PA197&dq=tdb+tt&source=web&ots=V0jT1Cnfsl&sig=LNVZxWoDhWWoe7_V1Sn07kZgcvM&hl=en&sa=X&oi=book_result&resnum=1&ct=result#PRA1-PA197,M1
-}


import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Astro.Time
import Test.QuickCheck
import Data.Time hiding (utc)
import Data.Time.Clock.TAI

-- Accuracies.
tdbError = 20 *~ micro second  -- About 10 us according to Kaplan, but for Vallado's cases the difference is greater...
dblError =  1 *~ nano second

ttConverges  = tt  1977 01 01 00 00 32.184 == tai 1977 01 01 00 00 00.000
tcgConverges = tcg 1977 01 01 00 00 32.184 == tai 1977 01 01 00 00 00.000
tdbConverges = abs (tdb 1977 01 01 00 00 32.184 `diffEpoch` tai 1977 01 01 00 00 00.000) < tdbError -- Don't think this should pass after closer investigation!

-- utc 1990 05 14 10 43 00.000 from [1].
valladoExample = abs (tt 1990 05 14 16 43 57.184 `diffEpoch` tdb 1990 05 14 16 43 57.18527) < tdbError
-- utc 2004 04 06 07 51 28.386009 from [2].
valladoExample2 = tai' == tt'
               && abs (diffEpoch tai' tcg') < dblError
               && abs (diffEpoch tai' tdb') < tdbError
               && abs (diffEpoch tdb' tcb') < dblError
  where
    tai' = tai 2004 04 06 7 52 00.386009
    tt'  = tt  2004 04 06 7 52 32.570009
    tcg' = tcg 2004 04 06 7 52 33.1695861742
    tdb' = tdb 2004 04 06 7 52 32.5716651154
    tcb' = tcb 2004 04 06 7 52 45.9109901113

main = do
  quickCheck ttConverges
  quickCheck tcgConverges
  quickCheck valladoExample
  quickCheck valladoExample2
  -- quickCheck tdbConverges -- TODO: fails!  
