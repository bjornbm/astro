module Astro.Coords where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import Numeric.Units.Dimensional.LinearAlgebra (elemSub, (<:), (<:.))
import Numeric.Units.Dimensional.LinearAlgebra.PosVel
import Numeric.Units.Dimensional.LinearAlgebra.Rotation (Homo33)


data Coord system a = C (CPos a)
                    | S (SPos a)
                    deriving (Show, Eq)

c :: Floating a => Coord s a -> CPos a
c (C v) = v
c (S v) = s2c v

s :: RealFloat a => Coord s a -> SPos a
s (S v) = v
s (C v) = c2s v

data Trans s1 s2 a = CC (CPos a -> CPos a)
                   | CS (CPos a -> SPos a)
                   | SC (SPos a -> CPos a)
                   | SS (SPos a -> SPos a)

apply :: RealFloat a => Trans s1 s2 a -> Coord s1 a -> Coord s2 a
apply (CC f) = C . f . c
apply (CS f) = S . f . c
apply (SC f) = C . f . s
apply (SS f) = S . f . s

diffCoords :: Floating a => Coord s a -> Coord s a -> CPos a
diffCoords c1 c2 = elemSub (c c1) (c c2)

-- Some coordinate systems

-- | The International Celestial Reference System (ICRS) has its
-- origin located at the barycenter of the solar system and its
-- axes fixed very near (within 0.1 arc second of) the mean equator
-- and equinox of J2000.
--
-- Per Wikipedia[1] "ICRS coordinates are approximately the same as equatorial
-- coordinates: the mean pole at J2000.0 in the ICRS lies at 17.3±0.2 mas in
-- the direction 12 h and 5.1±0.2 mas in the direction 18 h. The mean equinox
-- of J2000.0 is shifted from the ICRS right ascension origin by 78±10 mas
-- (direct rotation around the polar axis)."
--
-- [1]: http://en.wikipedia.org/wiki/International_Celestial_Reference_System
data ICRS = ICRS deriving (Show, Read)

-- | The Geocentric Celestial Reference System (GCRS) has its
-- origin at the center of Earth and axes parallel with the ICRS
data GCRS = GCRS deriving (Show, Read)

-- | The International Terrestrial Reference System (ITRS) is a an Earth fixed
-- rotating coordinate systems, connected to the ICRS by use of the IERS Earth
-- Orientation Parameters (EOP).
data ITRS = ITRS deriving (Show, Read) -- ITRS

type ECEF = ITRS  -- Losely defined.
type ECR = ITRS   -- Losely defined.

data Topocentric = Topocentric deriving (Show, Read)  -- Defined by geographic location.
data Orbital = Orbital deriving (Show, Read)  -- Defined by orbital state.

-- | Earth Centered Inertial. A losely defined “of Date” coordinate system.
-- Could be either Mean or True.
-- (TODO deprecate in favor of more rigorous systems.)
data ECI = ECI deriving (Show, Read)
type MEGSD = ECI  -- Close to ECI as per Soop, not very rigorous.

-- TODO Perhaps class ECI with instance GCRS, ToD, MEGSD, TEME... ?

-- Type synonyms for use elsewhere.
type Axis a = Vec3 DOne a
type CoordSys a = Homo33 DOne a

-- Cartesian axes
-- --------------
_X, _Y, _Z :: Num a => Axis a
_X = _1 <: _0 <:. _0
_Y = _0 <: _1 <:. _0
_Z = _0 <: _0 <:. _1
