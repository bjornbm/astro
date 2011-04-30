{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Module defining conversions between different orbit representations.
module Astro.Orbit.Conversion where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.LinearAlgebra.PosVel (CPos, CVel)
import qualified Prelude
import Astro.Orbit.SV
import Astro.Orbit.COE
import Astro.Orbit.COEm
import Astro.Orbit.MEOE
import Astro.Orbit.MEOEm
import Astro.Orbit.Types
import Astro.Orbit.Anomaly
import Data.AEq


-- | Convert state vector into Classical Orbital Elements. The
-- algorithm employed provides sound results for orbits with
-- inclination and/or eccentricity of zero.
--   i = 0   ->  RAAN = one of [-pi,-0,0,pi]
--   i = pi  ->  RAAN = pi
--   e = 0   ->  AoP  = 0
sv2coe :: RealFloat a => GravitationalParameter a -> CPos a -> CVel a -> COE a
sv2coe mu r' v' = COE
  { mu   = mu
  , slr  = p
  , ecc  = vNorm e'
  , inc  = inc
  , aop  = aop
  , raan = raan
  , trueAnomaly = TA trueAnomaly
  }
  where
    -- Angular momentum.
    h' = r' `crossProduct` v'  -- Angular momentum vector.
    h  = vNorm h'
    (h_x, h_y, h_z) = toTuple h'

    -- Semi-latus rectum
    p = h ^ pos2 / mu

    -- Inclination and ascending node.
    inc  = acos (h_z / h)
    raan = atan2 (h_x) (negate h_y)   -- pi for inc == 0.
    n' = cos raan <: sin raan <:. _0  -- Unit vector pointing towards AN.
    m' = h' `crossProduct` n'  -- Vector 90° ahead of n' (AN) in orbit plane.

    -- Eccentricity and perigee.
    e' = v' `crossProduct` h' >/ mu >-< vNormalize r'  -- See e.g. Wikipedia.
    aop = atan2 (e' >.< vNormalize m') (e' >.< n')     -- 0 for e' == 0'.

    -- Argument of latitude (angle from AN to r').
    u = atan2 (r' >.< vNormalize m') (r' >.< n')
    -- True anomaly.
    trueAnomaly = u - aop


-- | Convert a COE into a MEOE. Note that in general the true
-- longitude of the resultwill not be in the range [-pi,pi).
coe2meoe :: RealFloat a => COE a -> MEOE a
coe2meoe COE {..} = MEOE
  { mu = mu
  , p  = slr
  , f  = ecc * cos (aop + raan)
  , g  = ecc * sin (aop + raan)
  , h  = tan (inc / _2) * cos raan
  , k  = tan (inc / _2) * sin raan
  , trueLongitude = TL $ raan + aop + ta trueAnomaly  -- May be beyond [-pi,pi).
  }


-- | Convert a MEOE into a COE. Use the algoritm from Eagle
-- except for trueAnomaly for which it has a singularity.
-- Note that in general the true anomaly of the result will
-- not be in the range [-pi,pi).
meoe2coe :: RealFloat a => MEOE a -> COE a
meoe2coe MEOE{..} = COE
  { mu   = mu
  , slr  = p
  , ecc  = sqrt (f ^ pos2 + g ^ pos2)
  , inc  = atan2 (_2 * sqrt (h ^ pos2 + k ^ pos2)) (_1 - h ^ pos2 - k ^ pos2)
  , aop  = aop
  , raan = raan
  , trueAnomaly = TA $ l - aop - raan  -- May be beyond [-pi,pi).
  }
  where
    l = tl trueLongitude
    raan = atan2 k h
    aop  = atan2 (g * h - f * k) (f * h + g * k)


-- | Convert a MEOE to a cartesian State Vector. Algorithm from Eagle.
meoe2sv :: RealFloat a => MEOE a -> SV a
meoe2sv MEOE{..} = ( (r_x <: r_y <:. r_z) >* (r / s2)
                   , (v_x <: v_y <:. v_z) >* negate (sqrt (mu / p) / s2)
                   )
  where
    l = tl trueLongitude

    -- Position.
    r_x = cos l + a2 * cos l + _2 * h * k * sin l
    r_y = sin l - a2 * sin l + _2 * h * k * cos l
    r_z = _2 * (h * sin l - k * cos l)

    -- Velocity.
    v_x =         sin l  + a2 * sin l - hk2 * cos l + g - f * hk2 + a2 * g
    v_y = negate (cos l) + a2 * cos l + hk2 * sin l - f + g * hk2 + a2 * f
    v_z = negate _2 * (h * cos l + k * sin l + f * h + g * k)

    -- Auxilliaries used in the above.
    a2 = h ^ pos2 - k ^ pos2  -- alpha^2
    s2 = _1 + h ^ pos2 + k ^ pos2
    w  = _1 + f * cos l + g * sin l
    r  = p / w
    hk2 = _2 * h * k


-- | Convert Classical Orbital Elements into a State Vector.
coe2sv :: RealFloat a => COE a -> SV a
coe2sv = meoe2sv . coe2meoe



-- Mean Anomaly
-- ============

-- | Convert COE with true anomaly to COE with mean anomaly.
coe2coeM :: RealFloat a => COE a -> COEm a
coe2coeM COE{..} = COEm
  { mu   = mu
  , slr  = slr
  , ecc  = ecc
  , inc  = inc
  , aop  = aop
  , raan = raan
  , meanAnomaly = ta2ma (Ecc ecc) trueAnomaly
  }

-- | Convert COE with mean anomaly to COE with true anomaly.
coeM2coe :: (AEq a, RealFloat a) => COEm a -> COE a
coeM2coe COEm{..} = COE
  { mu   = mu
  , slr  = slr
  , ecc  = ecc
  , inc  = inc
  , aop  = aop
  , raan = raan
  , trueAnomaly = ma2ta (Ecc ecc) meanAnomaly
  }

-- | Convert a COEm into a MEOEm. Note that in general the mean
-- longitude of the resultwill not be in the range [-pi,pi).
coeM2meoeM :: RealFloat a => COEm a -> MEOEm a
coeM2meoeM COEm{..} = MEOEm
  { mu = mu
  , p  = slr
  , f  = ecc * cos (aop + raan)
  , g  = ecc * sin (aop + raan)
  , h  = tan (inc / _2) * cos raan
  , k  = tan (inc / _2) * sin raan
  , meanLongitude = ML $ raan + aop + ma meanAnomaly  -- May be beyond [-pi,pi).
  }

-- | Convert a COEm into a MEOEm. Note that in general the mean
-- longitude of the resultwill not be in the range [-pi,pi).
meoeM2coeM :: RealFloat a => MEOEm a -> COEm a
meoeM2coeM MEOEm{..} = COEm
  { mu   = mu
  , slr  = p
  , ecc  = sqrt (f ^ pos2 + g ^ pos2)
  , inc  = atan2 (_2 * sqrt (h ^ pos2 + k ^ pos2)) (_1 - h ^ pos2 - k ^ pos2)
  , aop  = aop
  , raan = raan
  , meanAnomaly = MA $ l - aop - raan  -- May be beyond [-pi,pi).
  }
  where
    l = ml meanLongitude
    raan = atan2 k h
    aop  = atan2 (g * h - f * k) (f * h + g * k)

-- | Convert MEOE with true anomaly to MEOE with mean anomaly.
meoe2meoeM :: RealFloat a => MEOE a -> MEOEm a
meoe2meoeM meoe@MEOE{..} = MEOEm
  { mu = mu
  , p  = p
  , f  = f
  , g  = g
  , h  = h
  , k  = k
  , meanLongitude = meanLongitude $ coeM2meoeM $ coe2coeM $ meoe2coe meoe
  }

-- | Convert MEOEm with mean anomaly to MEOE with true anomaly.
meoeM2meoe :: (AEq a, RealFloat a) => MEOEm a -> MEOE a
meoeM2meoe meoeM@MEOEm{..} = MEOE
  { mu = mu
  , p  = p
  , f  = f
  , g  = g
  , h  = h
  , k  = k
  , trueLongitude = trueLongitude $ coe2meoe $ coeM2coe $ meoeM2coeM meoeM
  }
