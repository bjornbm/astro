import Astro.Coords
import Astro.Place
import Astro.Place.ReferenceEllipsoid

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import PosVel hiding (latitude, longitude)
import Rotation (rotZ)
import qualified Prelude


long   = 47.5*~degree  -- SK long of IS-601.
rot601 = C . matVec (rotZ $ negate $ long + pi) . c  -- Rotate coord-sys so x-axis points from IS-601 to origo.

is601  = S $ fromTuple (42164*~kilo meter, 90*~degree, long)
is601' = rot601 is601


--eastSpotBeam  = s2c $ fromTuple (6370*~kilo meter, 90*~degree - 35.73*~degree, 35.60*~degree)
eastSpotBeam  = geodeticToECR $ GeodeticPlace wgs84 (35.73*~degree) (35.60*~degree) (0*~meter)
eastSpotBeam' = rot601 eastSpotBeam

x = (declination    diff /~degree,  -- elevation/latitude
     rightAscension diff /~degree)  -- longitude (perhaps azimuth for some definition...?)
diff = c2s $ elemSub (c eastSpotBeam') (c is601')
diff' = elemSub (c eastSpotBeam') (c is601')

