{-

What does it do?
----------------
Computes sensitivity of observed SC position due to constant errors
(biases) in range measurements.

What is it good for?
--------------------
If you are doing two-station ranging you can use one antenna as a
reference and solve for the bias on the other antennae. All other
things being equal the antenna for which the sensitivity
(dLongitude/dRange) is least should be chosen as reference. This
ensures that the impact of any unknown or incorrigible bias errors
on the antennae is minimized.

-}

{-
Requires safe, simpleargs.
-}

import Astro.Place
import Astro.Place.ReferenceEllipsoid
import Numeric.Units.Dimensional.Prelude
import Vector
import PosVel
import Tmp.ForwardAD
import Tmp.Lifts
import System.SimpleArgs -- System (getArgs)
--import System.Console.ParseArgs
import qualified Prelude
import Text.Printf
import Safe

type Longitude = Angle

-- | ECEF position of a perfectly geostationary SC.
perfectGEO :: RealFloat a => Longitude a -> CPos a
perfectGEO l = s2c $ fromTuple (r_GEO, 90*~degree, l)

-- | Geostationary radius.
r_GEO :: Num a => Length a
r_GEO = 42164 *~ kilo meter  -- Close enough.

-- | Computes the range from the given GS to a perfectly geostationary
-- SC at the given longitude.
range :: RealFloat a => GeodeticPlace a -> Longitude a -> Length a
range st l = vNorm $ elemSub (geodeticToCartesian st) (perfectGEO l)

-- | Determine whether the SC at the given longitude is above the horizon
-- of the given GS.
visible :: RealFloat a => GeodeticPlace a -> Longitude a -> Bool
visible st l = dotProduct stC r >= (0*~meter^pos2)
  where
    stC = geodeticToCartesian st
    r   = elemSub (perfectGEO l) stC


-- | Computes how sensitive the measured position of a geostationary SC is
-- to biases in range measurements from a GS. The arguments are the
-- geostationary longitude of the SC and the GS.
sensitivity :: RealFloat a => Longitude a -> GeodeticPlace a -> WaveNumber a
sensitivity l st = _1 / diff (range $ lift st) l


-- Ground stations
-- ---------------
type Station a = (String, GeodeticPlace a)

-- | Convenience function for defining ground stations.
station name long lat height = (name, GeodeticPlace wgs84 (long*~degree) (lat*~degree) (height*~kilo meter))

stations =
  [ station "LK" 43.23281  1.2069  0.23258
  , station "RK" 48.358678  1.7834  0.175
  , station "IEU" 49.92525036  9.92138889  0.192
  , station "FOT" 41.7851274352  13.601003  0.678311064  -- Fucino
  , station "PRE" (-25.73469603)  27.70694152  1.56141
  , station "HBK" (-25.88694) (360 Prelude.- 332.29242) 1.531
  , station "PET" (-31.634473860195)  115.887672  0.023597616  -- Perth
  , station "KSN" 35.94231031  127.48672289  0.1812
  , station "PAT" 21.539184941937  201.965712  0.15297912
  , station "NAP" 38.05804817  237.724  0.00792
  , station "FIL" 34.22691146  241.1071  0.341
  , station "LD" 33.80421832  241.57466667  0.0046
  , station "IPC" 33.61775124  242.9107485  0.53508
  , station "VAS" 36.23605  244.882  0.586  -- NLV
  , station "CRK" 39.08885821  255.1931333  2.1183
  , station "ATL" 33.48657775  275.7267  0.07367
  , station "CLK" 39.029938499358  282.729127  0.12095988  -- Clarksburg
  , station "MAS" 27.263  (360 Prelude.- 15.634)  0.167
  ]

-- | Computes sensitivities for a list of ground stations.
-- If the SC is below the GS's horizon Nothing is returned.
sensitivities :: RealFloat a => Longitude a -> [Station a] -> [(String, Maybe (WaveNumber a))]
sensitivities long = map (fmap (\st -> if visible st long then Just (sensitivity long st) else Nothing))

-- | Construct line describing SC location and error.
showSC :: Angle Double -> Angle Double -> String
showSC long longBias = printSC (long/~degree) (longBias/~degree) (longBias*r_GEO/~kilo meter)
  where printSC = printf ("GEO longitude: %7.2f   degE\n"
                       ++ "Error:         %9.4f degE  (%.3f km)")

-- | Construct line describing station sensitivity and estimated bias.
showStation :: Angle Double -> (String, Maybe (WaveNumber Double)) -> String
showStation longBias (name, Just s) = printf "%-7s  %9.3f  (%8.1f)  %9.3f" name (s/~(degree/kilo meter)) (s*r_GEO/~one) (negate longBias/s/~kilo meter)
showStation longBias (name, Nothing) = printf "%-7s           no visibility" name
  

main = do
  (long', station, bias') <- getArgs
  let !long = long' *~ degree
  let !bias = bias' *~ kilo meter

  let ss = sensitivities long stations
  let Just s1 = lookupJustDef (Just (0*~meter^neg1)) station ss
  let longBias = bias * s1  -- This linearization breaks down when SC and GS longitudes coincide.

  putStrLn $ showSC long longBias
  putStrLn $ ""
  putStrLn $ "             Sensitivity        Est. Bias" 
  putStrLn $ "Station  [degE/km]  ([kmE/km])    [km]"
  putStrLn $ "-------  ---------  ----------  ---------"
  putStrLn $ unlines $ map (showStation longBias) ss


{-
-- Version of main using parseargs library. Pretty nice except negative
-- args cannot be used (the are parsed as options/flags).
main2 = do
  args <- parseArgsIO ArgsComplete 
    [ argument Long (argDataRequired "longitude" ArgtypeDouble) "Geostationary satellite longitude [degE]"     -- Cannot be negative!
    , argument GS   (argDataRequired "GS"        ArgtypeString) "Reference ground station"
    , argument Bias (argDataRequired "bias"      ArgtypeDouble) "Bias error of reference ground station [km]"  -- Cannot be negative!
    ]
  let !long    = fromJustDef 0  (getArgDouble args Long) *~ degree
  let !station = fromJustDef "" (getArgString args GS)
  let !bias    = fromJustDef 0  (getArgDouble args Bias) *~ kilo meter

  let ss = sensitivities long stations
  let Just s1 = lookupJustDef (Just (0*~meter^neg1)) station ss
  let longBias = bias * s1  -- This linearization breaks down when SC and GS longitudes coincide.

  putStrLn $ showSC long longBias
  putStrLn $ ""
  putStrLn $ "             Sensitivity        Est. Bias" 
  putStrLn $ "Station  [degE/km]  ([kmE/km])    [km]"
  putStrLn $ "-------  ---------  ----------  ---------"
  putStrLn $ unlines $ map (showStation longBias) ss


data Options = Long | GS | Bias deriving (Ord, Eq, Show)
argument i a s = Arg i Nothing Nothing a s
-}

