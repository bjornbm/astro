module IAU2000.Equations where

import Astro
import Astro.Time
import IAU2000.FundamentalArguments (omega, c_k', a_k)
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


-- Other equations
-- ===============

-- | Mean obliquity of the ecliptic (the angle between the mean equator and
-- ecliptic, or equivalently, between the ecliptic pole and the mean
-- celestial pole of date).
--
-- Formula (5.12) of [Kaplan2005] or page B52 of [AsA2009].
meanObliquityOfEcliptic :: Floating a => E TT -> Angle a
meanObliquityOfEcliptic tt = ea_0 
  - 46.836769   *~ (arcsecond / century)      * t
  -  0.0001831  *~ (arcsecond / century^pos2) * t^pos2
  +  0.00200340 *~ (arcsecond / century^pos3) * t^pos3
  -  0.576e-6   *~ (arcsecond / century^pos4) * t^pos4
  -  0.434e-8   *~ (arcsecond / century^pos5) * t^pos5
  where
    ea_0 = 84381.406 *~ arcsecond
    t = sinceJ2000 tt


-- Equation of the equinoxes
-- =========================

-- | Equation of the equinoxes with accuracy on the order of
-- 0.5 microarcseconds during the interval 1975-2025 (assuming
-- accurate formulae are used for 'nutationInLongitude' and 
-- 'meanObliquityOfEcliptic'). 
-- From [AsA2009] page B10.
equationOfEquinoxes :: Floating a => E TT -> Astro a (Angle a)
equationOfEquinoxes tt = do
  nuts <- eval (angles.nutation) tt
  return $ fst nuts * cos (meanObliquityOfEcliptic tt)
         + sum (zipWith (*) c_k' $ fmap sin (a_k tt))
         + 0.00000087 *~ (arcsecond / century) * t * sin (omega tt)
  where t = sinceJ2000 tt

-- | Truncated equation of the equinoxes with accuracy on the order of
-- twenty microarcseconds (assuming accurate formulae are used for
-- 'nutationInLongitude' and 'meanObliquityOfEcliptic').
-- From [AsA2009] page B10.
equationOfEquinoxes' :: Floating a => E TT -> Astro a (Angle a)
equationOfEquinoxes' tt = do
  nuts <- eval (angles.nutation) tt
  return $ fst nuts * cos (meanObliquityOfEcliptic tt)
         + 0.00264*~arcsecond * sin (     omega tt)
         + 0.00006*~arcsecond * sin (_2 * omega tt)

