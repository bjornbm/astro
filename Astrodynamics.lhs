A collection of astrodynamics functionality. This modules is kind of
legacy and its functionality will hopefully eventually be migrated to
other modules.

> module Astrodynamics where

> import Numeric.Units.Dimensional.Prelude
> import Numeric.Units.Dimensional.NonSI (revolution)
> import Data.Time
> import Data.Time.Clock.TAI
> import Astro.Time
> import Astro.Time.Interop
> import qualified Astro.Time.Sidereal (gmst', phi)
> import Prelude 
>   ( fromIntegral, toRational, properFraction
>   , Fractional, RealFloat, Num, realToFrac
>   , ($), (.), (<), (++)
>   , map
>   )
> import qualified Prelude as P
> import Data.List (sort)

> {-

We define type synonyms to make the function signatures clearer.

> type Epoch = Time
> type MeanLongitude = Angle
> type DriftRate = AngularVelocity
> type EccentricityX = Dimensionless
> type EccentricityY = Dimensionless
> type InclinationX = Dimensionless
> type InclinationY = Dimensionless
> type RightAscension = Angle
> type Radius = Length
> type Longitude = Angle
> type Latitude = Angle
> ---}


Constants
=========

The speed of light in vacuum (Beta p. 509).

> c :: Fractional a => Velocity a
> c = 2.997925e8 *~ (meter / second)

The angular velocity of Earth's rotation.

> phi :: Floating a => AngularVelocity a
> phi = Astro.Time.Sidereal.phi

The gravitational parameter of Earth. Value from Wikipedia[3].

> mu :: Fractional a => GravitationalParameter a
> mu = 398600.4418 *~ (kilo meter ^ pos3 / second ^ pos2)

The ideal geostationary radius and velocity.

> r_GEO :: Floating a => Length a
> r_GEO = semiMajorAxis phi
> v_GEO :: Floating a => Velocity a
> v_GEO = phi * r_GEO

The sidereal day. See wikipedia[2] for other approximation.

> siderealDay :: Floating a => Time a
> siderealDay = 1 *~ revolution / phi


Functions
=========

Longitudes
----------
Calculates the right ascension of the Greenwich meridian at the epoch.

> greenwichRA t = Astro.Time.Sidereal.gmst' t

Calculates the right ascension of the longitude at the epoch. This 
This function is often used with a 'MeanLongitude' instead of a 
'Longitude'.

> longitudeRA t l = greenwichRA t + l
> raToLongitude t ra = ra - greenwichRA t

Calculate the time of day when the longitude has the specified right
ascension on the given day. On days when the longitude passes the right
ascension twice only the first occurence will be returned.

> longitudeToD :: RealFloat a => Angle a -> Angle a -> Day -> TimeOfDay
> longitudeToD l ra d = dayFractionToTimeOfDay . toRational $ dt /~ day
>   where
>       --t   = utcToTTTime (const 33) (UTCTime d midnight')
>       t   = mjd (fromInteger $ toModifiedJulianDay d) UT1
>       -- Greenwich Right Ascension at midnight.
>       ra0 = longitudeRA t l
>       dra = angleMod (ra - ra0)
>       -- Time required to accumulate dra.
>       dt  = dra / phi

Same for the Greenwich meridian.

> gwraToD :: RealFloat a => Angle a -> Day -> TimeOfDay
> gwraToD = longitudeToD _0

Orbits
------
Calculate the semi-major axis of an orbit based on the mean angular motion.

> semiMajorAxis :: Floating a => AngularVelocity a -> Length a
> semiMajorAxis n = cbrt (mu / n ^ pos2)

Calculate the mean angular motion of an orbit based on the semi-major axis.

> meanAngularMotion :: Floating a => Length a -> AngularVelocity a
> meanAngularMotion a = sqrt (mu / a ^ pos3)

Calculate the total orbital energy (kinetic + potential) per unit mass
based on the semi-major axis. The orbital energy is relative to the escape
velocity (E = 0 J/kg).

> orbitalEnergyPerUnitMass :: Floating a => Length a -> EnergyPerUnitMass a
> orbitalEnergyPerUnitMass a = negate mu / (_2 * a)

Drift rate
----------

Calculate drift rate of an orbit based on the orbital period.

> periodToDriftRate :: Floating a => Time a -> AngularVelocity a
> periodToDriftRate t = (1 *~ revolution) / t - phi

> driftRateToPeriod :: Floating a => AngularVelocity a -> Time a
> driftRateToPeriod d = tau / (d + phi)

> smaToDriftRate :: Floating a => Length a -> AngularVelocity a
> smaToDriftRate a = meanAngularMotion a - phi

> driftRateToSMA :: Floating a => AngularVelocity a -> Length a
> driftRateToSMA d = semiMajorAxis (phi + d)


Utility functions
=================

Limit an angle to within 0 and 2 pi.

> angleMod :: (RealFloat a) => Angle a -> Angle a
> angleMod x = (if f < 0 then (1 P.+ f) else f) *~ revolution 
>   where
>       (n, f) = properFraction (x /~ revolution)

A helper -- midnight as a 'DiffTime'.

> midnight' :: DiffTime
> midnight' = timeOfDayToTime midnight  -- Or "= 0"?


References
==========

[1] http://en.wikipedia.org/wiki/J2000
[2] http://en.wikipedia.org/wiki/Sideral_day
[3] http://en.wikipedia.org/wiki/Standard_gravitational_parameter


= Test stuff =

IS-11 ephemeris upload check.

> {-
> t = UTCTime (fromGregorian 2007 12 13) (timeOfDayToTime $ TimeOfDay 22 00 00)
> l = 316.8 *~ degree  -- SK longitude
> scra = greenwichRA t + l
> x = r_GEO * cos scra
> y = r_GEO * sin scra
> vx = r_GEO * phi * negate (sin scra) -- dx/dt
> vy = r_GEO * phi * cos scra          -- dy/dt
> -}

4367 longitude

> {-
> t = UTCTime (fromGregorian 2007 02 19) (timeOfDayToTime $ TimeOfDay 14 47 16)
> x = (-33304.86272037) *~ kilo meter
> y =   25960.48887207  *~ kilo meter
> z =     609.81557403  *~ kilo meter
> ra = atan2 y x
> l = angleMod $ ra - greenwichRA t
> -- -}
 

> {-
> long1  = 32.9 *~ degree
> day1   = fromGregorian 2006 04 18
> ra1    = (-88.2510367419438) *~ degree
> tod1   = longitudeToD long1 ra1 day1
> epoch1 = LocalTime day1 tod1

> ra2 = atan2 ((-0.009121) *~ radian) ((-0.000776) *~ radian)
> tod2   = longitudeToD long1 ra2 day1
> epoch2 = LocalTime day1 tod2
> -}


> {-

= 802 summer 2007 =

> day0 = toModifiedJulianDay (fromGregorian 2007 04 15)
> days = map (ModifiedJulianDay . (P.+ day0)) $ sort $ [7, 14 .. 136] ++ [3, 10 .. 136]

> raEpoch l ra d = LocalTime d (longitudeToD l ra d)

> epochs1 = map (raEpoch long1 ra1)
> epochs2 = map (raEpoch long1 ra2)
> epochs3 = map (raEpoch long1 (ra2 + dra))

> --days4 = map (ModifiedJulianDay . (P.+ day0)) $ sort $ [8, 16 .. 136] ++ [6, 14 .. 136] ++ [12, 20 .. 136]
> day4 = toModifiedJulianDay (fromGregorian 2007 04 25)
> days4 = map (ModifiedJulianDay . (P.+ day4)) [0, 2 .. 126]
> day5 = toModifiedJulianDay (fromGregorian 2007 05 17)
> days5 = map (ModifiedJulianDay . (P.+ day5)) [0, 2 .. 104]

> main2 = P.putStr . P.unlines $ ((map (P.show)) $ (epochs3 days))
> main = P.putStr . P.unlines $ ((map (P.show)) $ (epochs3 days5))



= Another attempt at summer 2007.

Inclination before and after free drift.

> (ixi_old, iyi_old) = ((-0.000481) *~ radian, (-0.000950) *~ radian)
> (ixf_old, iyf_old) = ((-0.000218) *~ radian,   0.000723  *~ radian)

Inclination change during free drift.

> (dix, diy) = (ixf_old - ixi_old, iyf_old - iyi_old)

To center origo during free drift we must have:

> (ixi, iyi) = (negate dix / _2, negate diy / _2)

Difference between old case and desired initial inclination vector.

> (dixi, diyi) = (ixi - ixi_old, iyi - iyi_old)

The total (RSS) difference in inclination between the old and the desired case.

> di = sqrt (dixi ^ pos2 + diyi ^ pos2)

How much do we have to change the DV angle to obtain the difference?

The total inclination change this summer.

> di_tot = 0.00847731696505944 *~ one

How much the angle must change to move di orthogonal to di_tot

> dra = atan (di / di_tot)

> -}
