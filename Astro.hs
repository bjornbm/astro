{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Astro where

import Astro.Time
import Control.Monad.Reader
import Data.Time.Clock.TAI (LeapSecondTable)
import Data.Fixed
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


--eval :: Astro a (AstroData a -> b -> c) -> b -> Astro a c
eval f x = asks (($x) . f)
evalM f x = asks f >>= ($x)


data TimeData = TimeData 
  { leapSecondTable :: LeapSecondTable
  , taiToUT1 :: E TAI -> E UT1
  , ut1ToTAI :: E UT1 -> E TAI
  ,  ttToTDB :: E TT  -> E TDB
  , tdbToTT  :: E TDB -> E TT
  }


data NutationModel a = NutationModel
  { angles      :: E TT -> (Angle a, Angle a)
  , equationOfEquinoxes :: E TT -> Astro a (Angle a)
  }

-- | The 'AstroData' data structure holds all astrophysical
-- constants. These so called constants aren't really constants (which is
-- why we don't just define them as constant functions and get on with
-- things) but are adjusted slightly based on observations by e.g. the
-- Naval Observatory. For example we store the leap second table in this
-- data structure.
data AstroData a = AstroData
  { time     :: TimeData
  , nutation :: NutationModel a
  }


-- | The 'Astro' monad allows us to access 'AstroData' using the
-- 'Reader'esque 'ask'. Usage e.g.:
--
--   runAstro (potentialEnergyPerUnitMass $ 42e6 *~ meter) defaultAstroData
--
type Astro a = Reader (AstroData a)
runAstro :: Astro a b -> AstroData a -> b
runAstro = runReader

