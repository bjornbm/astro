module IAU2000.TestEquations where

import Astro
import Astro.DefaultData
import Astro.Time
import Astro.Time.Convert
import Astro.Time.Sidereal
--import IAU2000.Equations
import Numeric.Units.Dimensional.Prelude
import Test.QuickCheck
import qualified Prelude

-- | Comparison allowing for inaccuracy.
cmpE :: (Ord a, Num a) => Quantity d a -> Quantity d a -> Quantity d a -> Bool
cmpE accuracy x x' = abs(x - x') < accuracy

run = (flip runAstro) defaultAstroData

-- Accuracies.
-- eeError = 1 *~ nano arcsecond
eeError = 1e-9 *~ arcsecond

--prop_ee_equals_gmst_minus_oo :: Double -> Bool
--prop_ee_equals_gmst_minus_oo t = run $ do

-- Test relationships between angles.
-- era = Earth Rotation Angle
-- eo  = Equation of Origins
-- ee  = Equation of Equinoxes

-- GAST = ERA - Eo
prop_gast_era_eo :: Double -> Bool
prop_gast_era_eo t = run $ do
  eo   <- equationOfOrigins tt
  gast <- gast tt
  era' <- convert tt >>= return . era
  return $ cmpE eeError gast (era' - eo)
  where tt = addTime j2000 (t *~ day)

-- Ee = GAST - GMST
prop_ee_gast_gmst :: Double -> Bool
prop_ee_gast_gmst t = run $ do
  ee   <- evalM (equationOfEquinoxes.nutation) tt
  gmst <- gmst tt
  gast <- gast tt
  return $ cmpE eeError ee (gast - gmst)
  where tt = addTime j2000 (t *~ day)

-- Eo = - (GMSTp + Ee)
prop_eo_gmstp_ee :: Double -> Bool
prop_eo_gmstp_ee t = run $ do
  eo    <- equationOfOrigins tt
  ee    <- evalM (equationOfEquinoxes.nutation) tt
  return $ cmpE eeError (negate eo) (gmst_p tt + ee)
  where tt = addTime j2000 (t *~ day)


main = do
  quickCheck prop_gast_era_eo
  quickCheck prop_ee_gast_gmst
  quickCheck prop_eo_gmstp_ee
