{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Test.Hspec.Monadic
import Test.Hspec.QuickCheck (property)
import Test.QuickCheck ((==>))
import Data.AEq

import TestUtil

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import qualified Prelude

import Astro.Orbit.COE
import Astro.Orbit.MEOE
import Astro.Orbit.SV
import Astro.Orbit.Conversion
import Astro.Orbit.Anomaly
import Astro.Orbit.Types



main = do
  hspec spec_fundamentals
  hspec spec_plusMinusPi
  hspec spec_plusTwoPi
  hspec spec_anomalyComparison
  hspec spec_anomalyConversion
  hspec spec_sv2coe
  hspec spec_coe2meoe2coe
  hspec spec_sv2coe2meoe2sv
  hspec spec_coe2coeM


-- | Verify some basic properties not strictly related to orbit representations.
spec_fundamentals = describe "Fundamentals" $ do

  it "atan2 y x + pi/2 ~= atan2 x (-y)"
    (property $ \(y::Dimensionless Double) x -> x /= _0 || y /= _0 ==>
      plusMinusPi (atan2 y x + pi / _2) ~== atan2 x (negate y))

  it "zero2one works as advertized"
    (property $ \x -> zero2one x >= _0 && zero2one x < _1)


-- ----------------------------------------------------------
spec_plusMinusPi = describe "plusMinusPi" $ do

  it "plusMinusPi -2*pi = 0"
    (plusMinusPi (negate _2 * pi) == _0)

  it "plusMinusPi -pi = -pi"
    (plusMinusPi (negate pi) == negate pi)

  it "plusMinusPi 0 = 0"
    (plusMinusPi _0 == _0)

  it "plusMinusPi pi = -pi"
    (plusMinusPi pi == negate pi)

  it "plusMinusPi 2pi = 0"
    (plusMinusPi (_2*pi) == _0)

  it "plusMinusPi x = x for x in [-pi,pi)"
    (property $ \x' -> let x = zero2one x' * _2 * pi - pi in plusMinusPi x ~== x)

  it "plusMinusPi returns values in [-pi,pi)"
    (property $ \x -> plusMinusPi x > negate pi && plusMinusPi x <= (pi::Angle Double))

  it "plusMinusPi x + 2 pi = plusMinusPi x"
    (property $ \x -> plusMinusPi (x + _2 * pi) ~== (plusMinusPi x::Angle Double))


-- ----------------------------------------------------------
spec_plusTwoPi = describe "plusTwoPi" $ do

  it "plusTwoPi -2*pi = 0"
    (plusTwoPi (negate _2 * pi) == _0)

  it "plusTwoPi -pi = pi"
    (plusTwoPi (negate pi) == pi)

  it "plusTwoPi 0 = 0"
    (plusTwoPi _0 == _0)

  it "plusTwoPi pi = pi"
    (plusTwoPi pi == pi)

  it "plusTwoPi 2*pi = 0"
    (plusTwoPi (_2 * pi) == _0)

  it "plusTwoPi x = x for x in [0,2*pi)"
    (property $ \x' -> let x = zero2one x' * _2 * pi in plusTwoPi x ~== x)

  it "plusTwoPi returns values in [0,2*pi)"
    (property $ \x -> plusTwoPi x >= _0 && plusTwoPi x < (_2 * pi::Angle Double))

  it "plusTwoPi x + 2 pi = plusTwoPi x"
    (property $ \x -> plusTwoPi (x + _2 * pi) ~== (plusTwoPi x::Angle Double))


-- ----------------------------------------------------------
spec_anomalyComparison = describe "Anomaly comparisons" $ do

  it "-pi and pi should be equal"
    (Anom (negate pi) == Anom pi)

  it "-pi and pi should be approximately equal"
    (Anom (negate pi::Angle Double) ~== Anom pi)

  it "0 and 2*pi should be equal"
    (Anom _0 == Anom (_2*pi))

  it "0 and 2*pi should be approximately equal"
    (Anom _0 ~== Anom (_2*pi::Angle Double))

  it "x and x+2*pi should be equal."
    (property $ \t -> Anom t ~== Anom (t + _2*pi::Angle Double))


-- ----------------------------------------------------------
spec_anomalyConversion = describe "Anomaly conversions" $ do

  it "Two ways of computing eccentric anomaly from true anomaly"
    (property $ \e' t -> let e = zero2one e'
      in eccAnomaly1 e t ~== eccAnomaly2 e t)

  it "Converting TA to EA and back should not change it."
    (property $ \e' t -> let e = Ecc $ zero2one e'
      in (ea2ta e . ta2ea e) t ~== t)

  it "At perigee TA and EA should be equally 0."
    (property $ \e' -> let e = Ecc $ zero2one e'
      in ta2ea e ta0 == ea0 && ea2ta e ea0 == ta0)

  it "At apogee TA and EA should be equally pi."
    (property $ \e' -> let e = Ecc $ zero2one e'
      in ta2ea e (Anom pi) ~== Anom pi && ea2ta e (Anom pi) == Anom pi)

  it "For circular orbit TA and EA should be equal."
    (property $ \a -> ta2ea e0 (Anom a) ~== Anom a && ea2ta e0 (Anom a) ~== Anom a)

  it "Converting EA to MA and back should not change it."
    (property $ \e' ea -> let e = Ecc $ zero2one e'
      in (ma2ea e . ea2ma e) ea ~== ea)

  it "At perigee EA and MA should be equally 0."
    (property $ \e' -> let e = Ecc $ zero2one e'
      in ea2ma e ea0 == ma0 && ma2ea e ma0 == ea0)

  it "At apogee EA and MA should be equally pi."
    (property $ \e' -> let e = Ecc $ zero2one e'
      in ea2ma e (Anom pi) == Anom pi && ma2ea e (Anom pi) == Anom pi)

  it "For circular orbit EA and MA should be equal."
    (property $ \a -> ea2ma e0 (Anom a) ~== Anom a && ma2ea e0 (Anom a) ~== Anom a)

  where
    e0 = Ecc _0 :: Eccentricity Double
    ta0 = Anom _0 :: Anomaly True Double
    ea0 = Anom _0 :: Anomaly Ecc  Double
    ma0 = Anom _0 :: Anomaly Mean Double


-- ----------------------------------------------------------
spec_coe2meoe2coe = describe "coe2meoe2coe" $ do

  it "Converting a COE to a MEOE and back to a COE does not change it"
    (coe2vec testCOE0 ~== (coe2vec . meoe2coe . coe2meoe) testCOE0)

  it "Converting a COE to a MEOE and back to a COE does not change it"
    (coe2vec testCOE1 ~== (coe2vec . meoe2coe . coe2meoe) testCOE1)

  it "Converting a COE (generated from a random SV) to a MEOE and back to a COE does not change it"
    (property $ \mu r v -> let coe = sv2coe mu r v :: COE True Double; i = inc coe
      in mu > 0*~(meter^pos3/second^pos2) && i /= pi && i /= negate pi
      ==> coe2vec coe ~== (coe2vec . meoe2coe . coe2meoe) coe
    )


-- ----------------------------------------------------------
spec_sv2coe2meoe2sv = describe "sv2coe2meoe2sv" $ do

  it "Converting a prograde SV to a MEOE and back to a SV does not change it"
    ((meoe2sv . coe2meoe . sv2coe') testSV0 ~== testSV0)

  it "Converting a retrograde SV to a MEOE and back to a SV does not change it – surprisingly!"
    ((fudgeSV . meoe2sv . coe2meoe . sv2coe') testSV0R ~== fudgeSV testSV0R)

  it "Converting a prograde SV to a MEOE and back to a SV does not change it"
    ((fudgeSV . meoe2sv . coe2meoe . sv2coe') testSV1 ~== fudgeSV testSV1)

  it "Converting a prograde SV to a MEOE and back to a SV does not change it"
    ((meoe2sv . coe2meoe . sv2coe') testSV2 ~== testSV2)

  it "Converting a retrograde SV to a MEOE and back to a SV does not change it – surprisingly!"
    ((meoe2sv . coe2meoe . sv2coe') testSV2R ~== testSV2R)

  it "Converting a prograde SV to a MEOE and back to a SV does not change it significantly"
    ((fudgeSV . meoe2sv . coe2meoe . sv2coe') testSV3 ~== fudgeSV testSV3)

  it "Converting a prograde SV to a MEOE and back to a SV does not change it significantly"
    ((fudgeSV . meoe2sv . coe2meoe . sv2coe') testSV4 ~== fudgeSV testSV4)

  it "Converting a random SV to a MEOE and back to a SV does not change it"
    (property $ \mu r v -> let coe = sv2coe mu r v :: COE True Double; i = inc coe
      in mu > 0*~(meter^pos3/second^pos2) && i /= pi && i /= negate pi
      ==> (r,v) ~== (meoe2sv $ coe2meoe $ sv2coe mu r v)
    )


-- ----------------------------------------------------------
spec_sv2coe = describe "sv2coe" $ do

  it "Inclination of prograde orbit in xy-plane is zero"
    (inc (sv2coe' testSV0) == _0)

  it "Inclination of retrograde orbit in xy-plane is pi"
    (inc (sv2coe' testSV0R) == pi)

  it "Inclination of orbit in xy-plane is zero (prograde) or pi (retrograde)"
    (property $ \mu x y vx vy -> mu > 0 *~ (meter ^ pos3 / second ^ pos2) ==>
      let r =  x <:  y <:. 0 *~ meter
          v = vx <: vy <:. 0 *~ mps
          i = inc (sv2coe mu r v) :: Angle Double
      in i == _0 || i == pi
    )

  it "RAAN of prograde orbit in xy-plane is pi"
    (raan (sv2coe' testSV0) == pi)

  it "RAAN of retrograde orbit in xy-plane is pi"
    (raan (sv2coe' testSV0R) == pi)

  it "RAAN of orbit in xy-plane is ±pi or zero"
    (property $ \mu x y vx vy -> mu > 0 *~ (meter ^ pos3 / second ^ pos2) ==>
      let r =  x <:  y <:. 0 *~ meter
          v = vx <: vy <:. 0 *~ mps
          ra = raan (sv2coe mu r v) :: Angle Double
      in ra ~== pi || ra ~== negate pi || ra == _0
    )

  it "For prograde orbit at perigee trueAnomaly = 0"
    (anomaly (sv2coe' testSV0) == Anom _0)

  it "For retrograde orbit at perigee trueAnomaly = 0"
    (anomaly (sv2coe' testSV0R) == Anom _0)

  it "For prograde orbit at apogee trueAnomaly = -pi"
    (anomaly (sv2coe' testSV1) == Anom (negate pi))

  it "For retrograde orbit at apogee trueAnomaly = pi"
    (anomaly (sv2coe' testSV1R) == Anom pi)

  it "Prograde orbit with AN, perigee, and anomaly coinciding on +x"
    (let coe = sv2coe' testSV2
      in raan coe == _0 && aop coe == _0 && anomaly coe == Anom _0
    )

  it "Retrograde orbit with AN, perigee, and anomaly coinciding on +x"
    (let coe = sv2coe' testSV2R
      in raan coe == _0 && aop coe == _0 && anomaly coe == Anom _0
    )

  it "Prograde orbit with DN, perigee, and anomaly coinciding on +x"
    (let coe = sv2coe' testSV3
      in raan coe == negate pi && aop coe == pi && anomaly coe == Anom _0
    )

  it "Prograde orbit with DN, apogee, and anomaly coinciding on +x"
    (let coe = sv2coe' testSV4
      in raan coe == negate pi && aop coe ~== _0 && anomaly coe == Anom pi
    )


-- ----------------------------------------------------------
spec_coe2coeM = describe "coe2meoe2coe" $ do

  it "Converting a COE to a COEm and back to a COE does not change it"
    (coe2vec testCOE0 ~== (coe2vec . coeM2coe . coe2coeM) testCOE0)

  it "Converting a COE to a COEm and back to a COE does not change it"
    (coe2vec testCOE1 ~== (coe2vec . coeM2coe . coe2coeM) testCOE1)

  {-
  -- This doesn't work for hyperbolic orbits(?).
  it "Converting a COE (generated from a random SV) to a COEm and back to a COE does not change it"
    (property $ \mu r v -> let coe = sv2coe mu r v :: COE Double; i = inc coe
      in mu > 0*~(meter^pos3/second^pos2) && i /= pi && i /= negate pi
      ==> coe2vec coe ~== (coe2vec . coeM2coe . coe2coeM) coe
    )
  -}

-- Convenience and utility functions.

mps = meter / second

-- | From Wikipedia.
mu_Earth = 398600.4418 *~ (kilo meter ^ pos3 / second ^ pos2)

-- | Removes the integral part of a value so that it ends up in the
-- interval [0,1).
zero2one :: Dimensionless Double -> Dimensionless Double
zero2one x = x - fromIntegral (floor (x /~ one)) *~ one

{-
-- | Constrains a value to the range [-pi,pi) by adding/subtracting
-- full revolutions.
plusMinusPi :: Angle Double -> Angle Double
plusMinusPi x = zero2one ((x + pi) / rev) * rev - pi
  where rev = _2 * pi
-}

-- | Compute eccentric anomaly using atan.
eccAnomaly1 :: Dimensionless Double -> Angle Double -> Angle Double
eccAnomaly1 e t = _2 * atan (sqrt ((_1 - e) / (_1 + e)) * tan (t / _2))
-- | Compute eccentric anomaly using atan2.
eccAnomaly2 :: Dimensionless Double -> Angle Double -> Angle Double
eccAnomaly2 e t = atan2 (sqrt (_1 - e ^ pos2) * sin t) (e + cos t)

-- | Convert an SV to a COE assuming Earth is central body.
sv2coe' = uncurry (sv2coe mu_Earth)

-- | Fudge a state vector to avoid comparing to zero elements
-- where the deviation may be greated than epsilon.
fudgeSV :: SV Double -> SV Double
fudgeSV (r,v) = (r >+< (a<:a<:.a), v >+< (b<:b<:.b))
  where
    a = vNorm r / (1e-9*~one)
    b = vNorm v / (1e-9*~one)


-- Test elements.

testCOE0 :: COE True Double
testCOE0 = COE
  { mu = mu_Earth
  , slr = 10000 *~ kilo meter
  , ecc = 0 *~ one
  , inc = 0 *~ degree
  , aop = 0 *~ degree
  , raan = 0 *~ degree
  , anomaly = Anom $ 0 *~ degree
  }

testCOE1 :: COE True Double
testCOE1 = COE
  { mu = mu_Earth
  , slr = 24000 *~ kilo meter
  , ecc = 0.01 *~ one
  , inc = 15 *~ degree
  , aop = (-105) *~ degree -- 255 *~ degree
  , raan = 35 *~ degree
  , anomaly = Anom $ 10 *~ degree
  }

testSV0 = (42156 *~ kilo meter <:    0 *~ meter <:. 0 *~ meter
          ,    0 *~ mps        <: 3075 *~ mps   <:. 0 *~ mps
          )

testSV0R = (42156 *~ kilo meter <:       0 *~ meter <:. 0 *~ meter
           ,    0 *~ mps        <: (-3075) *~ mps   <:. 0 *~ mps
           )

testSV1 = (42156 *~ kilo meter <:    0 *~ meter <:. 0 *~ meter
          ,    0 *~ mps        <: 3000 *~ mps   <:. 0 *~ mps
          )

testSV1R = (42156 *~ kilo meter <:       0 *~ meter <:. 0 *~ meter
           ,    0 *~ mps        <: (-3000) *~ mps   <:. 0 *~ mps
           )

testSV2 = ( 42156 *~ kilo meter <: 0 *~ meter <:. 0 *~ meter
          , 0 *~ mps <: 3075 *~ mps <:. 1 *~ mps
          )
testSV2R = ( 42156 *~ kilo meter <: 0 *~ meter <:. 0 *~ meter
           , 0 *~ mps <: (-3075) *~ mps <:. 1 *~ mps
           )

testSV3 = ( 42156 *~ kilo meter <: 0 *~ meter <:. 0 *~ meter
          , 0 *~ mps <: 3075 *~ mps <:. (-1) *~ mps
          )

testSV4 = ( 42156 *~ kilo meter <: 0 *~ meter <:. 0 *~ meter
          , 0 *~ mps <: 3000 *~ mps <:. (-1) *~ mps
          )



testCOEfail = sv2coe
  (0.4922582096724264 *~ (meter^pos3/second^pos2))
  (1.0011061239886767*~meter <: 0.3631363942943464*~meter <:. 0*~meter)
  ((-1.426229958784909)*~mps <: (-0.44155132615395254)*~mps <:. 0*~mps)

