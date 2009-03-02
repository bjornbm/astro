import Astro.Place
import Astro.Place.ReferenceEllipsoid
import Numeric.Units.Dimensional.Prelude
import Vector
import PosVel
import Tmp.ForwardAD
import Tmp.Lifts
import qualified Prelude

type Longitude = Angle

range :: RealFloat a => GeodeticPlace a -> Longitude a -> Length a
range st l = vNorm $ elemSub (geodeticToCartesian st) (perfectGEO l)
  where 
    perfectGEO :: RealFloat a => Longitude a -> CPos a
    perfectGEO l = s2c $ fromTuple (r_GEO, 90*~degree, l)

r_GEO :: Num a => Length a
r_GEO = 42164 *~ kilo meter


sensitivity :: RealFloat a => Longitude a -> GeodeticPlace a -> WaveNumber a
sensitivity l st = _1 / diff (range $ lift st) l

st1, st2, fot, pet :: Floating a => GeodeticPlace a
st1 = GeodeticPlace wgs84 (  15 *~degree) (30*~degree) (150*~meter)
st2 = GeodeticPlace wgs84 ((-35)*~degree) (20*~degree) (150*~meter)
fot = GeodeticPlace wgs84 (41.98*~degree) (13.6*~degree) (668*~meter)
pet = GeodeticPlace wgs84 ((-31.8)*~degree) (115.89*~degree) (24*~meter)

sc = 40.5*~degree
is706 = 50.25*~degree

type Station a = (String, GeodeticPlace a)

stations = [pet, fot]

