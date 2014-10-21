module Astro.AD (Lift (..), FAD) where

import Astro.Time (E (E))  -- For the Lift instance.
import Numeric.Units.Dimensional.AD

-- List instance for epochs. TODO Move!
instance Lift (E t) where lift (E t) = E (lift t)
