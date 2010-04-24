-- This sorry module is a holdall for small useful stuff while it waits
-- for a more appropriate place to live.
module Astro.Util (r_GEO, perfectGEO) where

import Astrodynamics (r_GEO)
import Vector
import PosVel
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


type Longitude = Angle

-- | ECEF position of a perfectly geostationary SC.
perfectGEO :: RealFloat a => Longitude a -> CPos a
perfectGEO l = s2c $ fromTuple (r_GEO, 90*~degree, l)