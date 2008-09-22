> module Astro where

The Astro monad allows convenient access to astrophysical constants within a monad. The drawback is the need for monadic code which tends to make simple formulae slightly wordy. An optional implementation could perhaps use implicit parameters?

> import qualified Prelude
> import Data.Time hiding (utc)
> import Data.Time.Clock.TAI
> import Data.Fixed (Pico)
> import Control.Monad.Reader
> import Numeric.Units.Dimensional.Prelude
> import Numeric.Units.Dimensional.NonSI (revolution)


We define type synonyms to make the function signatures clearer.

> type Epoch = AbsoluteTime
> type MeanLongitude = Angle
> type DriftRate = AngularVelocity
> type EccentricityX = Dimensionless
> type EccentricityY = Dimensionless
> type InclinationX = Dimensionless
> type InclinationY = Dimensionless
> type RightAscension = Angle
> type Radius = Length
> type Longitude = Angle
> type Latitude = Angle


Astrodynamics constants
-----------------------

| The 'AstroData' data structure holds all astrophysical constants. These so called constants aren't really constants (which is why we don't just define them as constant functions and get on with things) but are adjusted slightly based on observations by e.g. the Naval Observatory. For example we store the leap second table in this data structure.

> data AstroData a = AstroData
>   { c   :: Velocity a  -- The speed of light in vacuum.
>   , mu  :: GravitationalParameter a  -- The gravitational parameter of Earth.
>   , phi :: AngularVelocity a  -- Rate of Earth's rotation.
>   , greenwichRefEpoch :: Epoch
>   , greenwichRefRA :: Angle a
>   , leapSeconds :: LeapSecondTable
>   }


Time
----
The following functions make it easier to define and deal with times.

| A helper function to easily define epochs. The arguments are @y m d h
min s@ where the seconds can have a fractional part. Example usage:

  j2000 <- utc 2000 01 01 11 58 55.816  -- Wikipedia[1].

> utc :: Integer -> Int -> Int -> Int -> Int -> Pico -> Astro a Epoch
> utc y m d h min s = do
>   leapSeconds <- asks leapSeconds
>   return $ (utcToTAITime leapSeconds $ 
>       UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay h min s))

| Define a TAI epoch. Note that this function isn't monadic!

> tai :: Integer -> Int -> Int -> Int -> Int -> Pico -> Epoch
> tai y m d h min s = utcToTAITime (const 0) $
>       UTCTime (fromGregorian y m d) (timeOfDayToTime $ TimeOfDay h min s)

| Define a TT epoch. Note that this function isn't monadic!

> tt :: Integer -> Int -> Int -> Int -> Int -> Pico -> Epoch
> tt y m d h min s = addAbsoluteTime (-32.184) $ tai y m d h min s

| Computes the difference between the two epochs.

> diffTime :: Fractional a => Epoch -> Epoch -> Time a
> diffTime t1 t2 = realToFrac (diffAbsoluteTime t1 t2) *~ second

> j2000 = tt 2000 01 01 12 00 00.000
> sinceJ2000 t = diffTime t j2000


Defaults
--------

| A set of constants that should give decent results around 2008.

> defaultAstroData = AstroData
>   { c = 2.997925e8 *~ (meter / second)  -- Beta p. 509.
>   , mu = 398600.4418 *~ (kilo meter ^ pos3 / second ^ pos2)  -- ^ Wikipedia[3].
>   , phi = 360.985647  *~ (degree / day)  -- ^ Soop p. 7.
>   , greenwichRefEpoch = runAstro (utc 2007 01 01 0 0 0) defaultAstroData  -- ^ Soop p. 128.
>   , greenwichRefRA = 100.268 *~ degree  -- ^ Soop p. 128.
>   , leapSeconds = const 33  -- Valid for 2008.
>   }


Monad
-----

| The 'Astro' monad allows us to access 'AstroData' using the 'Reader'esque 'ask'.

> type Astro a = Reader (AstroData a)
> runAstro :: Astro a b -> AstroData a -> b
> runAstro = runReader

Usage e.g.:

  runAstro (potentialEnergyPerUnitMass $ 42e6 *~ meter) defaultAstroData




> {-
> longitudeRA :: Epoch -> Longitude a -> Astro a (RightAscension a)
> --longitudeRA t l = do
> --  f <- asks longitudeToRA
> --  return $ f t l
> longitudeRA = asks2 longitudeToRA

> asks1 f x   = do g <- asks f; return $ g x
> asks2 f x y = do g <- asks f; return $ g x y

> longitudeRA2 :: (?ad :: AstroData a) => Epoch -> Longitude a -> RightAscension a
> longitudeRA2 = longitudeToRA ?ad
> -}




