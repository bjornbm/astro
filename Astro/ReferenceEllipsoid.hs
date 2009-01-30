module Astro.ReferenceEllipsoid where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude

type EquatorialRadius  = Length
type PolarRadius       = Length
type Flattening        = Dimensionless
type InverseFlattening = Dimensionless


data ReferenceEllipsoid a = ReferenceEllipsoid
  { equatorialRadius :: EquatorialRadius a
  , polarRadius      :: PolarRadius a
  } deriving (Show, Eq)

flattening :: Fractional a => ReferenceEllipsoid a -> Flattening a
flattening e = _1 - polarRadius e / equatorialRadius e

inverseFlattening :: Fractional a => ReferenceEllipsoid a -> InverseFlattening a
inverseFlattening e = _1 / flattening e

-- | Creates a reference ellipsoid based on the given equatorial radius
-- and inverse flattening.
referenceEllipsoid' :: Fractional a => EquatorialRadius a -> InverseFlattening a -> ReferenceEllipsoid a
--  g = 1 / f  and  f = 1 - b / a  ==>  b = a * (1 - 1 / g) = a - a / g
referenceEllipsoid' a g = ReferenceEllipsoid a b where b = a - a / g

wgs84, grs80 :: Fractional a => ReferenceEllipsoid a

clarke1866        = ReferenceEllipsoid  (6378206.4  *~meter) (6356583.8*~meter)  -- [wp].
bessel1841        = referenceEllipsoid' (6377397.155*~meter) (299.1528153513233*~one)
hayford1910       = referenceEllipsoid' (6378388.0  *~meter) (297*~one)          -- [wp].
international1924 = hayford1910                                                  -- [wp].
krasovsky1940     = referenceEllipsoid' (6378245    *~meter) (298.3*~one)        -- [wp].
grs80             = referenceEllipsoid' (6378137.0  *~meter) (298.257222101*~one)  -- [wp].
wgs84             = referenceEllipsoid' (6378137.0  *~meter) (298.257223563*~one)  -- [WGS84].
iers2003          = referenceEllipsoid' (6378136.6  *~meter) (298.25642*~one)  -- Zero tide, [IERS2003].
sphere6371km      = ReferenceEllipsoid r r  where r = 6371000 *~ meter

{-
References
==========

[wp]        http://en.wikipedia.org/wiki/Figure_of_the_Earth
[WGS84]     http://earth-info.nga.mil/GandG/publications/tr8350.2/tr8350_2.html
[IERS2003]  http://www.iers.org/MainDisp.csl?pid=46-25776

-}

