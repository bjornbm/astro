module Astro.Time.Sidereal where

import Astro hiding (ut1ToTAI)
import Astro.Place
import Astro.Time
import Astro.Time.Convert
import Astro.Util.Cyclic -- (adjustZeroOne)
import Control.Monad.Reader
import Control.Applicative
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.NonSI (revolution)
import qualified Prelude as P


-- | Earth rotation angle, from AsA2009 B8.
  --
  -- This implementation has been optimized for better numerics
  -- than the naive implementation:
  --
  --  era' ut1 = 0.7790572732640     *~ revolution
  --           + 1.00273781191135448 *~ (revolution / day) * du
  --
era :: RealFloat a => E UT1 a -> Angle a
era ut1 = tau * ( 0.7790572732640 *~ one
                + fractionalPart (0.00273781191135448 *~ day ^ neg1 * du)
                + fractionalPart (du / (1 *~ day))
                )
  where
    du = sinceJ2000' ut1

-- | Earth's adopted mean angular velocity (the time derivative of the
  -- Earth rotation angle, see [AsA2009] B8).
  --
  -- prop> phi t == diff era t
phi :: Floating a => AngularVelocity a
phi = 1.00273781191135448 *~ (revolution / day)


-- | The polynomial part of GMST is almost entirely due to the effect of
-- precession and is given separately as it also forms part of the equation
-- of the origins [AsA2009].
--
-- According to [Kaplan2005] this polynomial is a function of TDB but for
-- this purpose TT can be used. [AsA2009] presents the formula as a function
-- of TT without even mentioning TDB.
gmst_p :: Floating a => E TT a -> Angle a
gmst_p tt = 0.014506   *~ arcsecond
          + 4612.156534*~(arcsecond/century)      * t
          + 1.3915817  *~(arcsecond/century^pos2) * t^pos2
          - 0.00000044 *~(arcsecond/century^pos3) * t^pos3
          - 0.000029956*~(arcsecond/century^pos4) * t^pos4
          - 3.68e-8    *~(arcsecond/century^pos5) * t^pos5
        where t = sinceJ2000 tt

-- | Greenwich mean sidereal time (GMST) expressed as an angle.
  -- The TT time should be consistent with the UT1 time. It is
  -- recommended to use `gmst` instead.
gmst_full :: RealFloat a => E UT1 a -> E TT a -> Angle a
gmst_full ut1 tt = era ut1 + gmst_p tt

-- | Greenwich mean sidereal time (GMST) expressed as an angle.
  -- This version disregards the difference between UT1 and TAI
  -- (about 35 s in 2014). The impact is less than one mas
  -- (milliarcsecond) from 1972 (start of TT scale) through 2050-ish.
  --
  -- GMST is consistent with Soop Table 1.
  --
gmst' :: RealFloat a => E UT1 a -> Angle a
gmst' ut1 = gmst_full ut1 (taiToTT $ ut1ToTAI (const _0) ut1)


-- Astro monad
-- ===========

-- | Greenwich mean sidereal time (GMST) as a function of TT epoch.
gmst :: RealFloat a => E TT a -> Astro a (Angle a)
gmst tt = do
  ut1 <- convert tt
  return $ gmst_full ut1 tt

-- | Greenwich apparent sidereal time (GAST) expressed as an angle.
--gast :: RealFloat a => E UT1 a -> E TT a -> Angle a
--gast ut1 tt = gmst ut1 tt + equationOfEquinoxes tt -- = era - equationOfOrigins??
gast :: RealFloat a => E TT a -> Astro a (Angle a)
gast tt = do  -- = era - equationOfOrigins??
  gmst <- gmst tt
  ee   <- evalM (equationOfEquinoxes.nutation) tt
  return $ gmst + ee

-- | Equation of origins.
--equationOfOrigins :: RealFloat a => E TT a -> Angle a
--equationOfOrigins tt = negate (gmst_p tt + equationOfEquinoxes tt)
equationOfOrigins :: RealFloat a => E TT a -> Astro a (Angle a)
equationOfOrigins tt = do
  ee <- evalM (equationOfEquinoxes.nutation) tt
  return $ negate (gmst_p tt + ee)



-- Local sideral time
-- ==================

-- | Local mean sidereal time (LMST). The first argument is the longitude
-- of the location of interest as measured from around the axis of the CIP
-- from the TIO (see [Kaplan2005]). This longitude is not the same as the
-- geodetic (ITRS) longitude, see 'longitudeCIP'!
lmstCIP :: RealFloat a
        => CIPLongitude a  -- ^ The longitude measured around CIP from TIO.
        -> E TT a
        -> Astro a (Angle a)
lmstCIP long tt = do
  gmst <- gmst tt
  return (gmst + long)

-- | Local mean sidereal time (LMST).
lmstITRS :: RealFloat a
         => GeodeticPlace a  -- ^ Geodetic (ITRS) place.
         -> E TT a
         -> Astro a (Angle a)
lmstITRS p tt = do
  (_,long) <- itrsToCIP p
  lmstCIP long tt

-- | Local apparent sidereal time (LAST). The first argument is the longitude
-- of the location of interest as measured from around the axis of the CIP
-- from the TIO (see [Kaplan2005]). This longitude is not the same as the
-- geodetic (ITRS) longitude, see 'longitudeCIP'!
lastCIP :: RealFloat a
        => CIPLongitude a  -- ^ The longitude measured around CIP from TIO.
        -> E TT a
        -> Astro a (Angle a)
lastCIP long tt = do
  gast <- gast tt
  return (gast + long)

-- | Local apparent sidereal time (LAST).
lastITRS :: RealFloat a
         => GeodeticPlace a  -- ^ Geodetic (ITRS) place.
         -> E TT a
         -> Astro a (Angle a)
lastITRS p tt = do
  (_,long) <- itrsToCIP p
  lastCIP long tt
