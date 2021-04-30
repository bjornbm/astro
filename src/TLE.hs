{-# LANGUAGE DataKinds #-}

module TLE where

import qualified Astrodynamics
import Numeric.Units.Dimensional.Prelude
import Numeric.NumType.DK.Integers (TypeInt(Zero, Neg1, Neg2, Neg3))
import Data.Time
import qualified Data.Text as T
import qualified Prelude


-- | The security classification of the data—all publicly available data
-- will be unclassified data.
data Classification = Classified Char
                    | Unclassified
                    | Secret
                    deriving (Show, Eq)

-- | A unique designation assigned by the World Data Center-A for Rockets and
-- Satellites (WDC-A-R&S) in accordance with international treaty (1975
-- Convention on Registration of Objects Launched into Outer Space).
data IntlDesignator = IntlDesignator
    { year   :: Integer  -- ^ Launch year
    , launch :: Int      -- ^ Launch number of the year
    , piece  :: T.Text     -- ^ Piece of the launch
    } deriving (Show, Eq)

-- | The type of ephemeris. Is expected to always be SGP4_SDP4.
data EphemerisType = SGP4_SDP4  -- 0
                   | SGP        -- 1
                   | SGP4       -- 2
                   | SDP4       -- 3
                   | SGP8       -- 4
                   | SDP8       -- 5
                   | EphemerisType Int  -- In case a new type materializes.
                   deriving (Show, Eq)

data TLE a = TLE
    { name           :: T.Text -- Name in NORAD catalog
    , noradID        :: Int  -- ^ NORAD catalog number
    , classification :: Classification
    , intlDesignator :: IntlDesignator
    , epoch          :: UTCTime  -- ^ The reference time for the element set.
    , dMdt           :: Quantity DMdt a    -- ^ Not used by SGP4/SDP4 models.
    , d2Mdt2         :: Quantity D2Mdt2 a  -- ^ Not used by SGP4/SDP4 models.
    , bStar          :: Quantity BStar a
    , ephemerisType  :: EphemerisType
    , elementNo      :: Int  -- ^ Normally incremented each time a new element
                             -- set is generated.
    , inclination    :: PlaneAngle a
    , raan           :: PlaneAngle a
    , eccentricity   :: Dimensionless a
    , aop            :: PlaneAngle a
    , meanAnomaly    :: PlaneAngle a
    , meanMotion     :: AngularVelocity a
    , revs           :: Int  -- ^ Revolution number at epoch.
    } deriving (Show, Eq)

-- | The first derivative of the mean motion.
type DMdt = Dim Zero Zero Neg2 Zero Zero Zero Zero
-- | The second derivative of the mean motion.
type D2Mdt2 = Dim Zero Zero Neg3 Zero Zero Zero Zero
-- | An SGP4-type drag coefficient.
type BStar = Dim Neg1 Zero Zero Zero Zero Zero Zero


period :: Floating a => TLE a -> Time a
period tle = _2 * pi * meanMotion tle ^ neg1
semiMajorAxis :: Floating a => TLE a -> Length a
semiMajorAxis = Astrodynamics.semiMajorAxis . meanMotion
driftRate :: Floating a => TLE a -> AngularVelocity a
driftRate = Astrodynamics.smaToDriftRate . semiMajorAxis
