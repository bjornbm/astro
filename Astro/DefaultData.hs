module Astro.DefaultData where

import Astro
import Astro.Time
import Astro.Time.Barycentric.Kaplan2005
import IAU2000.Nutation
import qualified IAU2000.Equations as Eq
import qualified Astro.TimeUT as UT


defaultTimeData = TimeData
  { leapSecondTable = const 33
  , taiToUT1 = \(E t) -> E t
  , ut1ToTAI = \(E t) -> E t
  ,  ttToTDB = convert  -- ttToTDB
  , tdbToTT  = convert  -- tdbToTT
  }

defaultNutationModel = NutationModel
  { angles              = nutationAngles2000A
  , equationOfEquinoxes = Eq.equationOfEquinoxes
  }

defaultAstroData = AstroData
  { time     = defaultTimeData
  , nutation = defaultNutationModel
  }

