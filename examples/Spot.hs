import Astro.Place
import Astro.Place.ReferenceEllipsoid

import Numeric.Units.Dimensional.Prelude
import Vector
import PosVel hiding (latitude, longitude)
import Matrix
import qualified Prelude


long   = 47.5*~degree  -- SK long of IS-601.
rot601 = matVec (rotZ $ negate $ long + pi)  -- Rotate coord-sys so x-axis points from IS-601 to origo.

is601  = s2c $ fromTuple (42164*~kilo meter, 90*~degree, long)
is601' = rot601 is601


--eastSpotBeam  = s2c $ fromTuple (6370*~kilo meter, 90*~degree - 35.73*~degree, 35.60*~degree)
eastSpotBeam  = geodeticToCartesian $ GeodeticPlace wgs84 (35.73*~degree) (35.60*~degree) (0*~meter)
eastSpotBeam' = rot601 eastSpotBeam

x = (elevation diff /~degree, azimuth diff /~degree) 
diff = c2s $ elemSub eastSpotBeam' is601'
diff' = elemSub eastSpotBeam' is601'

