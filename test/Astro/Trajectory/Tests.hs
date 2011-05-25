module Astro.Trajectory.Tests where

import Astro.Trajectory

import Numeric.Units.Dimensional.Prelude
import Astro.Time
import Astro.Orbit.Conversion
import Astro.Orbit.Conversion.Tests


m0 = meoe2meoeM $ coe2meoe testCOE0
m1 = meoe2meoeM $ coe2meoe testCOE1

t0 = mjd 10.0 UT1
t1 = mjd 11.0 UT1

et = ET [(t0,m0), (t1,m1)]



