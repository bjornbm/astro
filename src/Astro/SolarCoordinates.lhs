> module Astro.SolarCoordinates (solarCoordinates) where

> import Astro.Time
> import Astro.Coords
> import Numeric.Units.Dimensional.Prelude
> import Numeric.Units.Dimensional.LinearAlgebra
> import Numeric.Units.Dimensional.LinearAlgebra.PosVel (Sph (Sph), toZenith)
> import qualified Prelude


This module provides an algorithm for approximate solar coordinates
taken from the United States Naval Observatory (USNO) web site at
http://aa.usno.navy.mil/faq/docs/SunApprox.php. The narrative from
that page is included inline in the source for reference.

It is unclear what time system is expected for the JD, although UT1
is implied. It seems a bit strange to use UT1 as the Earth's orbit
is not coupled to its (axial) rotation, but with the provided
precision the difference between time scales is pretty insignificant,
and so pragmatism may be the reason for using UT1.

Furthermore the coordinate system of the computation result is not
specified. However, page C3 of the Astronomical Almanac (2009) implies
that the formulas on page C5 compute “ICRS geocentric equatorial”
coordinates, which I assume must strictly mean GCRS coordinates.

> -- | Computes the Sun's angular coordinates to an accuracy of about 1
> -- arcminute within two centuries of 2000. For more details see
> -- http://aa.usno.navy.mil/faq/docs/SunApprox.php.
> solarCoordinates :: RealFloat a => E ut1 a -> Coord GCRS a
> solarCoordinates ut1 = S $ Sph r (toZenith decl) ra
>   where

Given below is a simple algorithm for computing the Sun's angular
coordinates to an accuracy of about 1 arcminute within two centuries
of 2000. The algorithm's accuracy degrades gradually beyond its
four-century window of applicability. This accuracy is quite adequate
for computing, for example, the times of sunrise and sunset, or
solar transit. For navigational purposes it would provide about 1
nautical mile accuracy. The algorithm requires only the Julian date
of the time for which the Sun's coordinates are needed (Julian dates
are a form of Universal Time.)

First, compute D, the number of days and fraction (+ or –) from the
epoch referred to as "J2000.0", which is 2000 January 1.5, Julian
date 2451545.0:

      D = JD – 2451545.0

>     d = sinceJ2000' ut1
>       where sinceJ2000' = flip diffEpoch (clock' 2000 1  1 12 0 0)

where JD is the Julian date of interest. Then compute

Mean anomaly of the Sun:

      g = 357.529 + 0.98560028 D

>     g = 357.529 *~ degree + 0.98560028 *~ (degree/day) * d

Mean longitude of the Sun:

      q = 280.459 + 0.98564736 D

>     q = 280.459 *~ degree + 0.98564736 *~ (degree/day) * d

Geocentric apparent ecliptic longitude of the Sun (adjusted for
aberration):

      L = q + 1.915 sin g + 0.020 sin 2g

>     l = q + 1.915 *~ degree * sin g + 0.020 *~ degree * sin (_2 * g)

where all the constants (therefore g, q, and L)and are in degrees.
It may be necessary or desirable to reduce g, q, and L to the range
0° to 360°.

The Sun's ecliptic latitude, b, can be approximated by b=0. The
distance of the Sun from the Earth, R, in astronomical units (AU),
can be approximated by

      R = 1.00014 – 0.01671 cos g – 0.00014 cos 2g

>     r = 1.00014 *~ au - 0.01671 *~ au * cos g - 0.00014 *~ au * cos (_2 * g)
>           where au = astronomicalUnit

Once the Sun's apparent ecliptic longitude, L, has been computed,
the Sun's right ascension and declination can be obtained. First
compute the mean obliquity of the ecliptic, in degrees:

      e = 23.439 – 0.00000036 D

>     e = 23.439 *~ degree - 0.00000036 *~ (degree/day) * d

Then the Sun's right ascension, RA, and declination, d, can be
obtained from

      tan RA = cos e sin L / cos L
      sin d = sin e sin L

>     ra = atan2 (cos e * sin l) (cos l)
>     decl = asin (sin e * sin l)

RA is always in the same quadrant as L. If the numerator and
denominator on the right side of the expression for RA are used in
a double-argument arctangent function (e.g., "atan2"), the proper
quadrant will be obtained. If RA is obtained in degrees, it can be
converted to hours simply by dividing by 15. RA is conventionally
reduced to the range 0h to 24h.

Other quantities can also be obtained. The Equation of Time, EqT,
apparent solar time minus mean solar time, can be computed from

      EqT = q/15 – RA

where Eqt and RA are in hours and q is in degrees. The angular
semidiameter of the Sun, SD, in degrees, is simply

      SD = 0.2666 / R

This algorithm is essentially the same as that found on page C5 of
The Astronomical Almanac; a few constants have been adjusted above
to extend the range of years for which the algorithm is valid.

Graphs of the angular error of this algorithm can be viewed by
clicking on the links below. Each PDF graph shows the difference
in arcseconds between this algorithm and an accurate reference
ephemeris, as a function of year. The reference ephemeris is DE405
from the Jet Propulsion Laboratory.

-  Ecliptic coordinates (PDF, 254K) – longitude (red) and latitude (green)
-  Equatorial coordinates (PDF, 283K) – right ascension (red) and declination (green)


Other Sources:
--------------

If the algorithm given here is not sufficiently accurate, other
easy-to-implement ephemerides of the Sun (along with those of the
Moon and major planets) can be found in:

-  Chapront-Touze, M. & Chapront, J. 1991, Lunar Tables and Programs
   from 4000 BC to AD 8000 (Richmond, VA: Willmann-Bell, Inc.)

-  Bretagnon, P. & Simon, J-L 1986, Planetary Programs and Tables from
   –4000 to +2800 (Richmond, VA: Willmann-Bell, Inc.)

Alternatively, the Multiyear Interactive Computer Almanac (MICA),
an application for PCs and Macs, computes coordinates of the Sun,
Moon and planets to better than 0.1-arcsecond accuracy. It provides
almanac data in tabular form, at any time interval, for the years
1800 through 2050.
