-- Instances of Lift.

module Tmp.Lifts where

import Tmp.ForwardAD
import Astro.Place
import Astro.Place.ReferenceEllipsoid

instance Lift ReferenceEllipsoid where
  lift (ReferenceEllipsoid a b) 
    = ReferenceEllipsoid (lift a) (lift b)

instance Lift GeodeticPlace where
  lift (GeodeticPlace re lat long h) 
    = GeodeticPlace (lift re) (lift lat) (lift long) (lift h)

