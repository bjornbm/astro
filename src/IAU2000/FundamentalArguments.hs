module IAU2000.FundamentalArguments where

import Astro.Time
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


-- Fundamental Arguments
-- =====================
-- These are the fundamental arguments from chapter 5.4.2 of [Kaplan2005].
-- As an optimization(?) the arguments with exponents of t could be exploded
-- analogously to (5.8).

-- | Mean heliocentric ecliptic longitudes of the planets Mercury through
-- Neptune.
phi_1, phi_2, phi_3, phi_4, phi_5, phi_6, phi_7, phi_8 :: Floating a => E TT a -> Angle a
phi_1 tt =  908103.259872 *~ arcsecond + 538101628.688982 *~ (arcsecond/century) * t where t = sinceJ2000 tt
phi_2 tt =  655127.283060 *~ arcsecond + 210664136.433548 *~ (arcsecond/century) * t where t = sinceJ2000 tt
phi_3 tt =  361679.244588 *~ arcsecond + 129597742.283429 *~ (arcsecond/century) * t where t = sinceJ2000 tt
phi_4 tt = 1279558.798488 *~ arcsecond +  68905077.493988 *~ (arcsecond/century) * t where t = sinceJ2000 tt -- (5.17)
phi_5 tt =  123665.467464 *~ arcsecond +  10925660.377991 *~ (arcsecond/century) * t where t = sinceJ2000 tt
phi_6 tt =  180278.799480 *~ arcsecond +   4399609.855732 *~ (arcsecond/century) * t where t = sinceJ2000 tt
phi_7 tt = 1130598.018396 *~ arcsecond +   1542481.193933 *~ (arcsecond/century) * t where t = sinceJ2000 tt
phi_8 tt = 1095655.195728 *~ arcsecond +    786550.320744 *~ (arcsecond/century) * t where t = sinceJ2000 tt

-- | Approximation to general precession in longitude.
phi_9 :: Floating a => E TT a -> Angle a
phi_9 tt = 5028.8200 *~ (arcsecond/century) * t + 1.112022 *~ (arcsecond/century^pos2) * t^pos2 where t = sinceJ2000 tt -- (5.18)

-- | Mean anomaly of the Moon, /l/.
phi_10, l :: Floating a => E TT a -> Angle a
phi_10 tt =     485868.249036   *~  arcsecond
          + 1717915923.2178     *~ (arcsecond/century)      * t
          +         31.8792     *~ (arcsecond/century^pos2) * t^pos2
          +          0.051635   *~ (arcsecond/century^pos3) * t^pos3
          -          0.00024470 *~ (arcsecond/century^pos4) * t^pos4
        where t = sinceJ2000 tt
l = phi_10

-- | Mean anomaly of the Sun, /l'/.
phi_11, l' :: Floating a => E TT a -> Angle a
phi_11 tt =    1287104.79305    *~  arcsecond
          +  129596581.0481     *~ (arcsecond/century)      * t
          -          0.5532     *~ (arcsecond/century^pos2) * t^pos2
          +          0.000136   *~ (arcsecond/century^pos3) * t^pos3
          -          0.00001149 *~ (arcsecond/century^pos4) * t^pos4
        where t = sinceJ2000 tt
l' = phi_11

-- | Mean argument of latitude of the Moon, /F/.
phi_12, f :: Floating a => E TT a -> Angle a
phi_12 tt =     335779.526232   *~  arcsecond
          + 1739527262.8478     *~ (arcsecond/century)      * t
          -         12.7512     *~ (arcsecond/century^pos2) * t^pos2
          -          0.001037   *~ (arcsecond/century^pos3) * t^pos3
          +          0.00000417 *~ (arcsecond/century^pos4) * t^pos4
        where t = sinceJ2000 tt
f = phi_12

-- | Mean elongation of the Moon from the Sun, /D/.
phi_13, d :: Floating a => E TT a -> Angle a
phi_13 tt =    1072260.70369    *~  arcsecond
          + 1602961601.2090     *~ (arcsecond/century)      * t
          -          6.3706     *~ (arcsecond/century^pos2) * t^pos2
          +          0.006593   *~ (arcsecond/century^pos3) * t^pos3
          -          0.00003169 *~ (arcsecond/century^pos4) * t^pos4
        where t = sinceJ2000 tt
d = phi_13

-- | Mean longitude of the Moon's mean ascending node, /Omega/.
phi_14, omega :: Floating a => E TT a -> Angle a
phi_14 tt =     450160.398036   *~  arcsecond
          -    6962890.5431     *~ (arcsecond/century)      * t
          +          7.4722     *~ (arcsecond/century^pos2) * t^pos2
          +          0.007702   *~ (arcsecond/century^pos3) * t^pos3
          -          0.00005939 *~ (arcsecond/century^pos4) * t^pos4  -- (5.19)
        where t = sinceJ2000 tt
omega = phi_14

-- | Returns a list of all above fundamental arguments for purposes of
-- evaluating nutation series All of these arguments are used to evaluate
-- the planetary terms while only the first five are used to evaluate the
-- luni-solar terms. The order of arguments in the list is chosen to match
-- that in [3] rather than that used in [1].
fundamentalArguments :: Floating a => E TT a -> [Angle a]
fundamentalArguments tt = fmap ($ tt) [phi_10, phi_11, phi_12, phi_13, phi_14, phi_1, phi_2, phi_3, phi_4, phi_5, phi_6, phi_7, phi_8, phi_9]


-- | The arguments @A_k@ used in calculating the equation of the equinoxes
-- and the CIO locator @s@. Contains the arguments needed to calculate
-- all terms exceeding 0.5 microarcseconds during the interval 1975-2025.
a_k :: Floating a => E TT a -> [Angle a]
a_k tt = [                              omega tt
         ,                         _2 * omega tt
         , _2 * f tt - _2 * d tt + _3 * omega tt
         , _2 * f tt - _2 * d tt +      omega tt
         , _2 * f tt - _2 * d tt + _2 * omega tt
         , _2 * f tt             + _3 * omega tt
         , _2 * f tt             +      omega tt
         ,                         _3 * omega tt
         , l' tt + omega tt
         , l' tt - omega tt
         , l  tt + omega tt
         , l  tt - omega tt
         ]

-- | Coefficients required to calculate the CIO locator @s@ terms exceeding
-- 0.5 microarcseconds during the interval 1975-2025.
c_k :: Floating a => [Angle a]
c_k = [ -0.00264073
      , -0.00006353
      , -0.00001175
      , -0.00001121
      ,  0.00000457
      ] *~~ arcsecond ++ c_k_shared

-- | Coefficients required to calculate the equation of the equinoxes 
-- terms exceeding 0.5 microarcseconds during the interval 1975-2025.
c_k' :: Floating a => [Angle a]
c_k' = [ -0.00264096
       , -0.00006352
       , -0.00001175
       , -0.00001121
       ,  0.00000455
       ] *~~ arcsecond ++ c_k_shared

-- | Higher order coefficients that are identical for 'c_k' and 'c_k''.
c_k_shared :: Floating a => [Angle a]
c_k_shared = [ -0.00000202
             , -0.00000198
             ,  0.00000172
             ,  0.00000141
             ,  0.00000126
             ,  0.00000063
             ,  0.00000063
             ] *~~ arcsecond

