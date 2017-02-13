-- Instances of Lift.

module Tmp.Lifts where

import Numeric.Units.Dimensional.AD
import Astro.Place
import Astro.Place.ReferenceEllipsoid

instance Lift ReferenceEllipsoid where
  lift (ReferenceEllipsoid a b)
    = ReferenceEllipsoid (lift a) (lift b)

instance Lift GeodeticPlace where
  lift (GeodeticPlace re (GeodeticLatitude lat) (GeoLongitude long) h)
    = GeodeticPlace (lift re) (GeodeticLatitude $ lift lat)
                              (GeoLongitude     $ lift long)
                              (lift h)
