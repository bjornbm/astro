{-# LANGUAGE NoMonomorphismRestriction #-}

import Astro.Time
import Astro.TimeUT
import Astro.Time.Barycentric.Kaplan2005
import Numeric.Units.Dimensional.Prelude
import Data.Time.Clock (UTCTime)
import Text.Printf
import System (getArgs)
import qualified Prelude

clockUTC y m d h min s = convertToUTC (const 0) (clock y m d h min s TAI)

j2000utc = clockUTC 2000 01 01 12 00 00.000
j2000x = clock 2000 01 01 12 00 00.000

main = do
  [y,m,d,h,min,s] <- getArgs
  let utc = clockUTC (read y) (read m) (read d) (read h) (read min) (readFixed s)

  -- Read EOP data and define helpers for conversions.
  eopArray <- readFile "eop_test.txt" >>= return . mkEOPArray . parseEOPData
  let convertUTC = convertFromUTC (mkLeapSecondTable eopArray)
  let convertUT1 = convertToUT1 (mkUT1Table eopArray)

  -- | Print epochs.
  printf "UTC epoch: %s\n" (show utc)
  printf "TAI epoch: %s\n" $ show (convertUTC utc :: E TAI)
  printf "TT epoch:  %s\n" $ show (convertUTC utc :: E TT)
  printf "TCG epoch: %s\n" $ show (convertUTC utc :: E TCG)
  printf "TDB epoch: %s\n" $ show (convertUTC utc :: E TDB)
  printf "TCB epoch: %s\n" $ show (convertUTC utc :: E TCB)
  printf "UT1 epoch: %s\n" $ show $ convertUT1 (convertUTC utc :: E TAI)

  -- | Print seconds since J2000 in the respective time scales.
  printf "SI Seconds since J2000.0 UTC:  %f\n" $
    (diffEpoch (convertUTC utc) (convertUTC j2000utc :: E TAI) /~ second :: Double)
  printf "Days since J2000.0 UTC:  %f\n" $
    (diffEpoch (convertUTC utc) (convertUTC j2000utc :: E TAI) /~ day :: Double)
  printf "SI Seconds since J2000.0 TAI:  %f\n" $
    (diffEpoch (convertUTC utc) (j2000x TAI) /~ second :: Double)
  printf "SI Seconds since J2000.0 TT:   %f\n" $
    (diffEpoch (convertUTC utc) (j2000x TT) /~ second :: Double)
  printf "SI Seconds since J2000.0 TCG:  %f\n" $
    (diffEpoch (convertUTC utc) (j2000x TCG) /~ second :: Double)
  printf "TDB Seconds since J2000.0 TDB: %f\n" $
    (diffEpoch (convertUTC utc) (j2000x TDB) /~ second :: Double)
  printf "SI Seconds since J2000.0 TCB:  %f\n" $
    (diffEpoch (convertUTC utc) (j2000x TCB) /~ second :: Double)
  printf "UT1 Seconds since J2000.0 UT1: %f\n" $
    (diffEpoch (convertUT1 (convertUTC utc::E TAI)) (j2000x UT1) /~ second :: Double)

