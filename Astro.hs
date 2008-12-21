{-# LANGUAGE MultiParamTypeClasses #-}

module Astro where

import Astro.Time hiding (Convert)
import qualified Astro.TimeUT as UT
import Control.Monad.Reader
import Data.Time.Clock.TAI (LeapSecondTable)
import Data.Fixed
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


eval f x = asks (($x) . f)


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


-- Convert epochs.
class Convert t t' where convertt :: E t -> Astro a (E t')

instance Convert a a where convertt = return  -- Trivial.
instance Convert TAI UT1 where 
  convertt tai = asks (taiToUT1 . time) >>= return . ($ tai)

instance Convert TAI TT  where convertt = return . convert
instance Convert TAI TCG where convertt = return . convert

instance Convert TT  TAI where convertt =  return . convert
instance Convert TT  TCG where convertt =  return . convert

instance Convert TCG TAI where convertt = return . convert
instance Convert TCG TT  where convertt = return . convert

instance Convert TDB TCB where convertt = return . convert

instance Convert TCB TDB where convertt = return . convert



