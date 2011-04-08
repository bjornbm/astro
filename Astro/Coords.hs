module Astro.Coords where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import PosVel
import Numeric.Units.Dimensional.LinearAlgebra (elemSub)


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

diffCoords :: Floating a => Coord s a -> Coord s a -> Coord s a
diffCoords c1 c2 = C $ elemSub (c c1) (c c2)

-- Some coordinate systems
data ECI = ECI deriving (Show, Read) -- ICRS
data ECR = ECR deriving (Show, Read) -- ITRS
data Topocentric = Topocentric deriving (Show, Read)
data Orbital = Orbital deriving (Show, Read)
type MEGSD = ECI  -- Close to ECI as per Soop, not very rigorous.

-- Perhaps class ECI with instance ICRS, MEGSD, TEME...


