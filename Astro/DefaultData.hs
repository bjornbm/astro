module Astro.DefaultData where

import Astro
import Astro.Place.ReferenceEllipsoid
import Astro.Time hiding (taiToUT1, ut1ToTAI)
import qualified Astro.Time.Barycentric.Kaplan2005 as TDB
import IAU2000.Nutation
import qualified IAU2000.Equations as Eq
import Data.Time.Clock.AnnouncedLeapSeconds


defaultTimeData = TimeData
  { leapSecondTable = lst
  , taiToUT1 = coerce  -- Astro.Time.taiToUT1 (const 0) ??
  , ut1ToTAI = coerce
  ,  ttToTDB = TDB.ttToTDB
  , tdbToTT  = TDB.tdbToTT
  }

defaultNutationModel = NutationModel
  { angles              = nutationAngles2000A
  , equationOfEquinoxes = Eq.equationOfEquinoxes
  }

defaultAstroData = AstroData
  { time         = defaultTimeData
  , nutation     = defaultNutationModel
  , refEllipsoid = iers2003
  }

