module Astro.DefaultData where

import Astro
import Astro.Place.ReferenceEllipsoid
import Astro.Time hiding (taiToUT1, ut1ToTAI)
import qualified Astro.Time.Barycentric.Kaplan2005 as TDB
import IAU2000.Nutation
import qualified IAU2000.Equations as Eq
import Data.Time.Clock.AnnouncedLeapSeconds (lst)
import Data.Default

defaultTimeData :: Floating a => TimeData a
defaultTimeData = TimeData
  { leapSecondMap = lst
  , taiToUT1 = coerceE  -- Astro.Time.taiToUT1 (const 0) ??
  , ut1ToTAI = coerceE
  ,  ttToTDB = TDB.ttToTDB
  , tdbToTT  = TDB.tdbToTT
  }

defaultNutationModel :: Floating a => NutationModel a
defaultNutationModel = NutationModel
  { angles              = nutationAngles2000A
  , equationOfEquinoxes = Eq.equationOfEquinoxes
  }


defaultAstroData :: Floating a => AstroData a
defaultAstroData = AstroData
  { time         = defaultTimeData
  , nutation     = defaultNutationModel
  , refEllipsoid = iers2003
  }

-- TODO: How do I best avoid these being orphan instances?
instance Floating a => Default (TimeData a) where def = defaultTimeData
instance Floating a => Default (NutationModel a) where def = defaultNutationModel
instance Floating a => Default (AstroData a) where def = defaultAstroData
