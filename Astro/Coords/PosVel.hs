module Astro.Coords.PosVel where

import Astro.Coords
import Numeric.Units.Dimensional.LinearAlgebra.PosVel
import Numeric.Units.Dimensional.LinearAlgebra.VectorAD (applyLinear)


data PosVel system a = C' (CPos a) (CVel a)
                     | S' (SPos a) (SVel a)
                     deriving (Show, Eq)

pos :: PosVel s a -> Coord s a
pos (C' p _) = C p
pos (S' p _) = S p

cpos :: Floating a => PosVel s a -> CPos a
cpos (C' p _) = p
cpos (S' p _) = s2c p

spos :: RealFloat a => PosVel s a -> SPos a
spos (S' p _) = p
spos (C' p _) = c2s p

-- | Extract the cartesian velocity vector.
cvel :: RealFloat a => PosVel s a -> CVel a
cvel (C' _ v) = v
cvel (S' p v) = snd $ applyLinear s2c (p,v)

-- | Extract the spherical velocity.
svel :: RealFloat a => PosVel s a -> SVel a
svel (S' _ v) = v
svel (C' p v) = snd $ applyLinear c2s (p,v)

--applyLinearC f (C' p v) = let (p',v') = applyLinear f (p,v) in C' p' v'

