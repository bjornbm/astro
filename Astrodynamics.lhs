A collection of astrodynamics functionality.

> module Astrodynamics where

> import Buckwalter.Dimensional.Prelude
> import Buckwalter.Dimensional.NonSI (revolution)
> import Data.Time
> import Prelude 
>   ( fromIntegral, toRational, properFraction
>   , Fractional, RealFloat, Num, realToFrac
>   , ($), (.), (<), (++)
>   , map
>   )
> import qualified Prelude as P
> import Data.List (sort)


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


= Constants =

We keep all empirical physical constants and other "slowly changing"
data in a data structure.

> data Constants a = Constants 
>   { greenwichRefEpoch :: UTCTime
>   , greenwichRefAngle :: Angle a
>   , phi               :: AngularVelocity a
>   }

We define a default set of constants.

> defaultConstants = Constants

    The angular velocity of Earth's rotation (Soop p. 7).

>   { phi = 360.985647 *~ (degree / day)

    The Greenwich Right Ascension reference epoch and angle (Soop p. 128).
    These should ideally be updated on a yearly basis. TODO: Investigate
    options.

>   , greenwichRefEpoch = UTCTime (fromGregorian 2007 01 01) midnight'
>   , greenwichRefAngle = 100.268 *~ degree

>   }

> midnight' = timeOfDayToTime midnight


= Functions =

Calculates the right ascension of the Greenwich meridian at the epoch.

> greenwichRA c t = greenwichRefAngle c + phi c * dt
>   where
>       dt = utcToDimensional t - utcToDimensional (greenwichRefEpoch c)

Calculates the right ascension of the longitude at the epoch. This 
This function is often used with a 'MeanLongitude' instead of a 
'Longitude'.

> longitudeRA c t l = greenwichRA c t + l

Calculate the time of day when the longitude has the specified right
ascension on the given day. On days when the longitude passes the right
ascension twice only the first occurence will be returned.

> longitudeToD c l ra d = dayFractionToTimeOfDay . toRational $ dt /~ day
>   where
>       -- Greenwich Right Ascension at midnight.
>       ra0 = longitudeRA c (UTCTime d midnight') l
>       dra = angleMod (ra - ra0)
>       -- Time required to accumulate dra.
>       dt  = dra / phi c

Same for the Greenwich meridian.

> gwraToD c ra d = longitudeToD c (0 *~ degree)


= Utility functions =

Convert a UTCTime to a Dimensional representing the time elapsed since
MJD reference epoch. Leapseconds are not honored.

> utcToDimensional :: (Fractional a) => UTCTime -> Time a
> utcToDimensional (UTCTime (ModifiedJulianDay d) dt)
>   = fromIntegral d *~ day + realToFrac dt *~ second

Limit an angle to within 0 and 2 pi.

> angleMod :: (RealFloat a) => Angle a -> Angle a
> angleMod x = (if f < 0 then (1 P.+ f) else f) *~ revolution 
>   where
>       (n, f) = properFraction (x /~ revolution)


= Test stuff =

> long1  = 32.9 *~ degree
> day1   = fromGregorian 2006 04 18
> ra1    = (-88.2510367419438) *~ degree
> tod1   = longitudeToD defaultConstants long1 ra1 day1
> epoch1 = LocalTime day1 tod1

> ra2 = atan2 ((-0.009121) *~ radian) ((-0.000776) *~ radian)
> tod2   = longitudeToD defaultConstants long1 ra2 day1
> epoch2 = LocalTime day1 tod2


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
