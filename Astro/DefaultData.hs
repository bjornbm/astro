module Astro.DefaultData where

import Astro
import Astro.Time hiding (taiToUT1, ut1ToTAI)
import qualified Astro.Time.Barycentric.Kaplan2005 as TDB
import IAU2000.Nutation
import qualified IAU2000.Equations as Eq


defaultTimeData = TimeData
  { leapSecondTable = const 33
  , taiToUT1 = \(E t) -> E t  -- Astro.Time.taiToUT1 (const 0) ??
  , ut1ToTAI = \(E t) -> E t
  ,  ttToTDB = TDB.ttToTDB
  , tdbToTT  = TDB.tdbToTT
  }

defaultNutationModel = NutationModel
  { angles              = nutationAngles2000A
  , equationOfEquinoxes = Eq.equationOfEquinoxes
  }

defaultAstroData = AstroData
  { time     = defaultTimeData
  , nutation = defaultNutationModel
  }

