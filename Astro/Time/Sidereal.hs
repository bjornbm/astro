module Astro.Time.Sidereal where

import Astro
import Astro.Time
import Astro.Time.Convert
import Control.Monad.Reader
import IAU2000.Nutation
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (revolution)
import qualified Prelude as P


-- | Earth rotation angle.
era :: RealFloat a => E UT1 -> Angle a
era ut1 = 0.7790572732640    *~ revolution
        + 0.00273781191135448*~(revolution/day) * du + fracRev du
      where
        du = diffEpoch ut1 j2000ut1
        j2000ut1 = jd 2451545.0 UT1
        fracRev = (*~revolution) . snd . properFraction . (/~day)

-- | The polynomial part of GMST is almost entirely due to the effect of
-- precession and is given separately as it also forms part of the equation
-- of the origins [AsA2009].
--
-- According to [Kaplan2005] this polynomial is a function of TDB but for
-- this purpose TT can be used. [AsA2009] presents the formula as a function
-- of TT without even mentioning TDB.
gmst_p :: Floating a => E TT -> Angle a
gmst_p tt = 0.014506   *~ arcsecond
          + 4612.156534*~(arcsecond/century)      * t
          + 1.3915817  *~(arcsecond/century^pos2) * t^pos2
          - 0.00000044 *~(arcsecond/century^pos3) * t^pos3
          - 0.000029956*~(arcsecond/century^pos4) * t^pos4
          - 3.68e-8    *~(arcsecond/century^pos5) * t^pos5
        where t = diffEpoch tt j2000

-- | Greenwich mean sidereal time (GMST) expressed as an angle.
gmst' :: RealFloat a => E UT1 -> E TT -> Angle a
gmst' ut1 tt = era ut1 + gmst_p tt


-- Astro monad
-- ===========

-- | Greenwich mean sidereal time (GMST) as a function of TT epoch.
gmst :: RealFloat a => E TT -> Astro a (Angle a)
gmst tt = do
  ut1 <- convert tt
  return $ gmst' ut1 tt

-- | Greenwich apparent sidereal time (GAST) expressed as an angle.
--gast :: RealFloat a => E UT1 -> E TT -> Angle a
--gast ut1 tt = gmst ut1 tt + equationOfEquinoxes tt -- = era - equationOfOrigins??
gast :: RealFloat a => E TT -> Astro a (Angle a)
gast tt = do  -- = era - equationOfOrigins??
  gmst <- gmst tt
  ee   <- evalM (equationOfEquinoxes.nutation) tt
  return $ gmst + ee

-- | Equation of origins.
--equationOfOrigins :: RealFloat a => E TT -> Angle a
--equationOfOrigins tt = negate (gmst_p tt + equationOfEquinoxes tt) 
equationOfOrigins :: RealFloat a => E TT -> Astro a (Angle a)
equationOfOrigins tt = do
  ee <- evalM (equationOfEquinoxes.nutation) tt
  return $ negate (gmst_p tt + ee) 

