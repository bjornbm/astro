module Astro.DefaultData where

import Astro
import Astro.Time
import Astro.Time.Barycentric.Kaplan2005
import qualified IAU2000.IAU2000A as IAU2000A
import qualified Astro.TimeUT as UT


defaultTimeData = TimeData
  { leapSecondTable = const 33
  , taiToUT1 = \(E t)->E t
  , ut1ToTAI = undefined
  , ttToTDB = convert  -- ttToTDB
  , tdbToTT = convert  -- tdbToTT
  }

defaultNutationModel = NutationModel
  { angles      = IAU2000A.nutationAngles
  , equationOfEquinoxes = IAU2000A.equationOfEquinoxes''
  }

defaultAstroData = AstroData
  { time     = defaultTimeData
  , nutation = defaultNutationModel
  }

