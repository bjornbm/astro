{-

Based on [1]. The calculation of planetary and luni-solar terms are
kept separate as a optimization. Probably premature since I'm not
sure how much it buys us but is should save some 7000 multiplications
and additions per evaluation of the nutation parameters.

Any reference to chapters, sections, or equations are implicitly
referring to [1] unless otherwise specified.

[1] http://aa.usno.navy.mil/publications/docs/Circular_179.pdf

-}

module IAU2000.Nutation where

import Astro
import Astro.Time
import Numeric.Units.Dimensional.Prelude
import IAU2000.Table53
import IAU2000.FundamentalArguments (fundamentalArguments)
import Control.Monad.Reader
import qualified Prelude


-- Full Series
-- ===========

-- | Returns pairs of sines and cosines of the 678 luni-solar and 687
-- planetary @Phi_i@ terms from [1]. The @Phi_i@ terms are calculated
-- according to eq (5.16).
trigTerms :: Floating a => E TT -> [(Dimensionless a, Dimensionless a)]
trigTerms tt = fmap (toXY . sum . zipWith (*) args) multipliers
  where
    toXY x = (sin x, cos x)
    args = fundamentalArguments tt

-- | Returns the nutation angles @(DeltaPhi, DeltaEps)@ at the given epoch.
-- @DeltaPhi@ is the nutation in longitude and @DeltaEps@ is the nutation
-- in obliquity measured in the ecliptic system of date as described in
-- chapter 5.4.2 of [1].
-- The @Int@ argument is the number of terms to use in the nutation series.
nutationAngles :: Floating a => Int -> E TT -> (Angle a, Angle a)
nutationAngles n tt = (sum $ take n deltaPhiTerms, sum $ take n deltaEpsTerms) where
  deltaPhiTerms = zipWith (\(s, s_dot, c') (sinPhi, cosPhi) -> (s + s_dot * t) * sinPhi + c' * cosPhi)  phiCoeffs (trigTerms tt)
  deltaEpsTerms = zipWith (\(c, c_dot, s') (sinPhi, cosPhi) -> (c + c_dot * t) * cosPhi + s' * sinPhi)  epsCoeffs (trigTerms tt)
  t = sinceJ2000 tt


-- Variations of the series
-- ========================

-- | The full IAU 2000A nutation series. Calculates the direction of the
-- celestial pole in the GCRS with an accuracy of 0.2 mas ([Kaplan2005]
-- p.47).
nutationAngles2000A :: Floating a => E TT -> (Angle a, Angle a)
nutationAngles2000A = nutationAngles 1365 -- Could use 'maxBound'.

-- | The truncated IAU 2000B nutation series. Duplicates the full series
-- ('nutationAngles2000A') to within a milliarcsecond for input epochs
-- between 1995 and 2050 ([Kaplan2005] p.47).
nutationAngles2000B :: Floating a => E TT -> (Angle a, Angle a)
nutationAngles2000B = nutationAngles 77

-- | A truncated nutation series with 488 terms. Duplicates the full series'
-- ('nutationAngles2000A') to within 0.1 milliarcsecond accuracy between
-- 1700 and 2300. This emulates a subroutine provided by NOVAS
-- ([Kaplan2005] p.47).
nutationAngles488 :: Floating a => E TT -> (Angle a, Angle a)
nutationAngles488 = nutationAngles 488

