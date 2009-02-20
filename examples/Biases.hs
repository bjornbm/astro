import Astro.Place
import Astro.Place.ReferenceEllipsoid
import Numeric.Units.Dimensional.Prelude
import Vector
import PosVel
import ForwardAD
import qualified Prelude

type Longitude = Angle

range :: RealFloat a => GeodeticPlace a -> Longitude a -> Length a
range st l = vNorm $ elemSub (geodeticToCartesian st) (perfectGEO l)
  where 
    perfectGEO :: RealFloat a => Longitude a -> CPos a
    perfectGEO l = s2c $ fromTuple (r_GEO, 90*~degree, l)

r_GEO :: Num a => Length a
r_GEO = 42164 *~ kilo meter

st1 = GeodeticPlace wgs84 (  15 *~degree) (30*~degree) (150*~meter)
st2 = GeodeticPlace wgs84 ((-35)*~degree) (20*~degree) (150*~meter)

