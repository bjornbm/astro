{- |
Module for dealing with latitude and longitude. I expect to find
a more suitable place too put these eventually but for now this will
do.
-}
module Astro.Place where

import Astro
import Astro.Place.ReferenceEllipsoid
import Control.Monad.Reader (asks)
import Numeric.Units.Dimensional.Prelude
import Vector
import PosVel
import qualified Prelude

-- | Geodetic height above mean sea level.
type GeodeticHeight     = Length
-- | Geodetic latitude. North latitudes are positive.
type GeodeticLatitude   = Angle
-- | Geocentric latitude. North latitudes are positive.
type GeocentricLatitude = Angle
-- | Geocentric radius.
type GeocentricRadius   = Length
-- | Geodetic and geocentric longitude (they are equivalent). East longitudes are positive.
type GeoLongitude       = Angle
-- | Longitude measured around the axis of the CIP from the TIO meridian.
type CIPLongitude       = Angle
-- | Latitude measured around the axis of the CIP from the TIO meridian.
type CIPLatitude        = Angle

-- | Geodetic latitude, longitude and height bundled with a reference
-- ellipsoid. (Without the context of a reference ellipsoid the geodetic
-- coordinates aren't of much utility.)
data GeodeticPlace a = GeodeticPlace
  { refEllips :: ReferenceEllipsoid a
  , latitude  :: GeodeticLatitude   a
  , longitude :: GeoLongitude       a
  , height    :: GeodeticHeight     a
  }

-- | Converts geodetic longitude and latitude in the ITRS frame into
-- geocentric longitude and latitude in the CIP/TIO frame. The formulae
-- used are approximate and should not be used for places at polar
-- latitudes. See [AsA2009] page B84.
--
-- (Should this take geodetic vs geocentric latitude into account? Would
-- that fix polar latitudes? Or should polar motionrotation matrix be
-- used for accurate results? It is also unclear whether the latitude
-- in the CIP/TIO frame is geocentric or geodetic... does the concept
-- of geodetic exist in that frame?)
itrsToCIP :: Floating a 
          => GeodeticPlace a
          -> Astro a (CIPLatitude a, CIPLongitude a)
itrsToCIP p = do
  xp <- undefined :: Astro a (Angle a) -- TODO
  yp <- undefined :: Astro a (Angle a) -- TODO
  let lat'  = lat  + (xp * cos long - yp * sin long)
      long' = long + (xp * sin long + yp * cos long) * tan lat
  return (lat',long')
  where 
    lat  = latitude p
    long = longitude p



-- | Converts a geodetic place into geocentric cartesian coordinates.
-- From [WP1].
geodeticToCartesian :: Floating a
                    => GeodeticPlace a
                    -> CPos a
geodeticToCartesian p = fromTuple (x,y,z)
  where
    -- Inputs. (Could have pattern-matched instead.)
    a    = equatorialRadius $ refEllips p
    b    =      polarRadius $ refEllips p
    lat  = latitude  p
    long = longitude p
    h    = height    p
    -- Intermediate calculations.
    e  = _1 - b ^ pos2 / a ^ pos2
    xi = sqrt (_1 - e ^ pos2 * sin lat ^ pos2)
    -- Final results.
    x = (a / xi + h) * cos lat * cos long
    y = (a / xi + h) * cos lat * sin long
    z = (a * (_1 - e^pos2) / xi + h) * sin lat

{-
This function doesn't make much sense. Just use 'c2s'.

-- | Converts geodetic longitude, latitude and height into geocentric
-- longitude, latitude and radius.
geodeticToGeocentric :: RealFloat a
                     => GeodeticPlace a
                     -> (GeocentricLatitude a, GeoLongitude a, GeocentricRadius a)
geodeticToGeocentric p = (dec, ra, r)
  where (r, ra, dec) = toTuple $ c2s $ geodeticToCartesian p
-}

{- 
Conversion from geocentric to geodetic is semi-complex. Will implement
only if needed. For details see [WP2].


References
==========

* [AsA2009]   Astronomical Almanac for the Year 2009

* [WP1]       <http://en.wikipedia.org/wiki/Geodetic_system#From_geodetic_to_ECEF_coordinates>

* [WP2]       <http://en.wikipedia.org/wiki/Geodetic_system#From_geodetic_to_ECEF_coordinates>

-}
