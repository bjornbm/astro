{-

Based on [1]. The calculation of planetary and luni-solar terms are
kept separate as a optimization. Probably premature since I'm not
sure how much it buys us but is should save some 7000 multiplications
and additions per evaluation of the nutation parameters.

Any reference to chapters, sections, or equations are implicitly
referring to [1] unless otherwise specified.

[1] http://aa.usno.navy.mil/publications/docs/Circular_179.pdf
[2]
[3] http://maia.usno.navy.mil:80/conv2003/chapter5/tab5.3b.txt 

-}

module IAU2000.IAU2000A where

import Astro -- (Epoch, sinceJ2000)
import Numeric.Units.Dimensional.Prelude
import IAU2000.Tab53a
import IAU2000.Tab53b
import qualified Prelude


-- Fundamental Arguments
-- =====================
-- These are the fundamental arguments from chapter 5.4.2. As an
-- optimization(?) the arguments with exponents of t could be exploded
-- analogously to (5.8).

-- | Mean heliocentric ecliptic longitudes of the 
-- planets Mercury through Neptune.
phi_1 t =  908103.259872 *~ radian + 538101628.688982 *~ (radian/century) * t 
phi_2 t =  655127.283060 *~ radian + 210664136.433548 *~ (radian/century) * t 
phi_3 t =  361679.244588 *~ radian + 129597742.283429 *~ (radian/century) * t 
phi_4 t = 1279558.798488 *~ radian +  68905077.493988 *~ (radian/century) * t  -- (5.17)
phi_5 t =  123665.467464 *~ radian +  10925660.377991 *~ (radian/century) * t 
phi_6 t =  180278.799480 *~ radian +   4399609.855732 *~ (radian/century) * t 
phi_7 t = 1130598.018396 *~ radian +   1542481.193933 *~ (radian/century) * t 
phi_8 t = 1095655.195728 *~ radian +    786550.320744 *~ (radian/century) * t

-- | Approximation to general precession in longitude.
phi_9 t = 5028.8200 *~ (radian/century) * t + 1.112022 *~ (radian/century^pos2) * t^pos2  -- (5.18)

-- | Mean anomaly of the Moon, /l/.
phi_10 t =     485868.249036   *~  radian
         + 1717915923.2178     *~ (radian/century)      * t
         +         31.8792     *~ (radian/century^pos2) * t^pos2
         +          0.051635   *~ (radian/century^pos3) * t^pos3
         -          0.00024470 *~ (radian/century^pos4) * t^pos4

-- | Mean anomaly of the Sun, /l'/.
phi_11 t =    1287104.79305    *~  radian
         +  129596581.0481     *~ (radian/century)      * t
         -          0.5532     *~ (radian/century^pos2) * t^pos2
         +          0.000136   *~ (radian/century^pos3) * t^pos3
         -          0.00001149 *~ (radian/century^pos4) * t^pos4

-- | Mean argument of latitude of the Moon, /F/.
phi_12 t =     335779.526232   *~  radian
         + 1739527262.8478     *~ (radian/century)      * t
         -         12.7512     *~ (radian/century^pos2) * t^pos2
         -          0.001037   *~ (radian/century^pos3) * t^pos3
         +          0.00000417 *~ (radian/century^pos4) * t^pos4

-- | Mean elongation of the Moon from the Sun, /D/.
phi_13 t =    1072260.70369    *~  radian
         + 1602961601.2090     *~ (radian/century)      * t
         -          6.3706     *~ (radian/century^pos2) * t^pos2
         +          0.006593   *~ (radian/century^pos3) * t^pos3
         -          0.00003169 *~ (radian/century^pos4) * t^pos4

-- | Mean longitude of the Moon's mean ascending node, /Omega/.
phi_14 t =     450160.398036   *~  radian
         -    6962890.5431     *~ (radian/century)      * t
         +          7.4722     *~ (radian/century^pos2) * t^pos2
         +          0.007702   *~ (radian/century^pos3) * t^pos3
         -          0.00005939 *~ (radian/century^pos4) * t^pos4  -- (5.19)

-- | Returns a list of all fundamental arguments. All of these
-- arguments are used to evaluate the planetary terms while only the
-- first five are used to evaluate the luni-solar term. The order of
-- arguments in the list is chosen to match that in [3] rather than
-- that used in [1].
arguments t = fmap ($t) [phi_10, phi_11, phi_12, phi_13, phi_14, phi_1, phi_2, phi_3, phi_4, phi_5, phi_6, phi_7, phi_8, phi_9]


-- Luni-Solar Terms
-- ================

-- | Returns pairs of sines and cosines of the 678 luni-solar @Phi_i@ terms
-- from [1]. The @Phi_i@ terms are calculated according to eq (5.16).
luniSolarTrigTerms :: Floating a => Time a -> [(Dimensionless a, Dimensionless a)]
luniSolarTrigTerms t = fmap ((\x -> (sin x, cos x)) . sum . zipWith (*) args) luniSolarMultipliers
  where args = arguments t

-- | Returns the pair @(DeltaPhi_ls, DeltaEps_ls)@ at the given time.
luniSolarNutationContribution :: Floating a => Time a -> (Angle a, Angle a)
luniSolarNutationContribution t = (sum deltaPhiTerms, sum deltaEpsTerms) where
  deltaPhiTerms = zipWith (\(s, s_dot, c') (sinPhi, cosPhi) -> (s + s_dot * t) * sinPhi + c' * cosPhi)  luniSolarPhiCoeffs (luniSolarTrigTerms t)
  deltaEpsTerms = zipWith (\(c, c_dot, s') (sinPhi, cosPhi) -> (c + c_dot * t) * cosPhi + s' * sinPhi)  luniSolarEpsCoeffs (luniSolarTrigTerms t)


-- Planetary Terms
-- ===============

-- | Returns pairs of sines and cosines of the 687 planetary @Phi_i@ terms
-- from [1]. The @Phi_i@ terms are calculated according to eq (5.16).
planetaryTrigTerms :: Floating a => Time a -> [(Dimensionless a, Dimensionless a)]
planetaryTrigTerms t = fmap ((\x -> (sin x, cos x)) . sum . zipWith (*) args) planetaryMultipliers
  where args = arguments t

-- | Returns the pair @(DeltaPhi_p, DeltaEps_p)@ at the given time.
planetaryNutationContribution :: Floating a => Time a -> (Angle a, Angle a)
planetaryNutationContribution t = (sum deltaPhiTerms, sum deltaEpsTerms) where
  deltaPhiTerms = zipWith (\(s, c') (sinPhi, cosPhi) -> s * sinPhi + c' * cosPhi)  planetaryPhiCoeffs (planetaryTrigTerms t)
  deltaEpsTerms = zipWith (\(c, s') (sinPhi, cosPhi) -> c * cosPhi + s' * sinPhi)  planetaryEpsCoeffs (planetaryTrigTerms t)


-- Full Series
-- ===========
-- Here we finally combine the contributions from luni-solar and planetary terms.

-- | Returns the nutation angles @(DeltaPhi, DeltaEps)@ at the given epoch.
-- @DeltaPhi@ is the nutation in longitude and @DeltaEps@ is the nutation
-- in obliquity measured in the ecliptic system of date as described in
-- chapter 5.4.2 of [2]. Using these parameters the direction of the celestial
-- pole in the GCRS can be calculated with an accuracy of 0.2 mas.
nutationAngles :: Floating a => Epoch -> (Angle a, Angle a)
nutationAngles epoch = (dp_ls + dp_p, de_ls + de_p) where
  t = sinceJ2000 epoch
  (dp_ls, de_ls) = luniSolarNutationContribution t
  (dp_p , de_p ) = planetaryNutationContribution t

-- | Mean obliquity of the ecliptic, /eps/, at given epoch.
meanObliquityOfEcliptic :: Floating a => Epoch -> Angle a
meanObliquityOfEcliptic epoch = eps_0 
  - 46.836769     *~ (arcsecond / century)      * t
  -  0.0001831    *~ (arcsecond / century^pos2) * t^pos2
  +  0.00200340   *~ (arcsecond / century^pos3) * t^pos3
  -  0.000000576  *~ (arcsecond / century^pos4) * t^pos4
  -  0.0000000434 *~ (arcsecond / century^pos5) * t^pos5
  where
    eps_0 = 84381.406 *~ arcsecond
    t = sinceJ2000 epoch




