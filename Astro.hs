module Astro where

import Astro.Time hiding (Convert)
import qualified Astro.TimeUT (UT1, UTC)
import qualified Astro.TimeUT as UT
import Astro.Time.Barycentric.Kaplan2005
import Control.Monad.Reader
import Data.Time.Clock.TAI
import Data.Fixed
import Numeric.Units.Dimensional.Prelude
import qualified Prelude


data TimeData = TimeData 
  { leapSecondTable :: LeapSecondTable
  , taiToUT1 :: E TAI -> E UT.UT1
  , ut1ToTAI :: E UT.UT1 -> E TAI
  ,  ttToTDB :: E TT  -> E TDB
  , tdbToTT  :: E TDB -> E TT
  }

defaultTimeData = TimeData
  { leapSecondTable = const 33
  , taiToUT1 = \(E t)->E t
  , ut1ToTAI = undefined
  , ttToTDB = convert  -- ttToTDB
  , tdbToTT = convert  -- tdbToTT
  }


data AstroData a = AstroData
  { time :: TimeData
  }

defaultAstroData = AstroData
  { time = defaultTimeData
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
instance Convert TAI UT.UT1 where 
  convertt tai = asks (taiToUT1 . time) >>= return . ($ tai)

instance Convert TAI TT  where convertt = return . convert
instance Convert TAI TCG where convertt = return . convert

instance Convert TT  TAI where convertt =  return . convert
instance Convert TT  TCG where convertt =  return . convert

instance Convert TCG TAI where convertt = return . convert
instance Convert TCG TT  where convertt = return . convert

instance Convert TDB TCB where convertt = return . convert

instance Convert TCB TDB where convertt = return . convert



