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
Requires simpleargs.
-}

import Astro.AD.Place
import Astro.Coords
import Astro.Place
import Astro.Place.ReferenceEllipsoid
import Astro.Place.Topocentric
import Astro.Util (perfectGEO, r_GEO, sexagesimalAngle)
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.LinearAlgebra
import Numeric.Units.Dimensional.AD
import System.Environment (getArgs)
import qualified Prelude
import Data.Maybe (fromMaybe)
import Control.Monad (join)
import Text.Printf


-- | Determine whether the SC at the given longitude is above the horizon
-- of the given GS.
visible :: RealFloat a => GeodeticPlace a -> GeoLongitude a -> Bool
visible st l = elevation' st (perfectGEO l) >= _0


-- | Computes how sensitive the measured position of a geostationary SC is
-- to biases in range measurements from a GS. The arguments are the
-- geostationary longitude of the SC and the GS.
sensitivity :: RealFloat a => GeoLongitude a -> GeodeticPlace a -> WaveNumber a
sensitivity l st = _1 / diff (range' (lift st) . perfectGEO . GeoLongitude) (geoLongitude l)


-- Ground stations
-- ---------------
type Station a = (String, GeodeticPlace a)

-- | Convenience function for defining ground stations.
station name lat long height = (name, GeodeticPlace wgs84 (GeodeticLatitude $ lat*~degree) (GeoLongitude $ long*~degree) (height*~kilo meter))

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
  , ("Cibinong", cibinong)
  , station "Banjarmasin" (-3.37) 114.6 0
  , station "Dasoguz"  41.50 59.55 0.086
  , station "Ashgabat" 38.14 58.37 0.134
  , station "Follingbo" 57.59269 18.37298  86.53  -- Eniro.se + STA altitude
  , station "Esrange" 67.885 21.0788 327.50 -- STA
  , station "Chilworth" 50.961 (-1.428) 327.50 -- Approximation from Google Maps
  , station "Al Ghuwayriyah" 25.33 51.28 0  -- TODO
  , station "Tunis" 36.806389 10.181667 0  -- TODO
  , station "Daegu" 35.866667 128.6 0  -- Wikipedia city location
  , station "Daejeon" 36.351 127.385 0  -- Wikipedia city location
  ]

cibinong = GeodeticPlace wgs84 (GeodeticLatitude $ negate $ sexagesimalAngle 6 26 52) (GeoLongitude $ sexagesimalAngle 106 56 10) _0

-- | Computes sensitivities for a list of ground stations.
-- If the SC is below the GS's horizon Nothing is returned.
sensitivities :: RealFloat a => GeoLongitude a -> [Station a] -> [(String, Maybe (WaveNumber a))]
sensitivities long = map (fmap (\st -> if visible st long then Just (sensitivity long st) else Nothing))

-- | Construct line describing SC location and error.
showSC :: GeoLongitude Double -> Angle Double -> String
showSC long longBias = printSC (geoLongitude long /~ degree)
                               (longBias /~ degree)
                               (longBias * r_GEO /~ kilo meter)
  where printSC = printf ("GEO longitude: %7.2f   degE\n"
                       ++ "Error:         %9.4f degE  (%.3f km)")

-- | Construct line describing station sensitivity and estimated bias.
showStation :: Angle Double -> (String, Maybe (WaveNumber Double)) -> String
showStation longBias (name, Just s)  = printf "%-14s  %9.3f  (%8.1f)  %9.3f" name (s/~(degree/kilo meter)) (s*r_GEO/~one) (negate longBias/s/~kilo meter)
showStation longBias (name, Nothing) = printf "%-14s           no visibility" name


main = getArgs >>= \args -> case args of  -- Could use LambaCase
  [long, station, bias] -> main' long station bias
  _                     -> do
    putStrLn "Computes sensitivity of observed SC position due to constant errors"
    putStrLn "(biases) in range measurements."
    putStrLn ""
    putStrLn "    Usage: <executable> longitude station bias"
    putStrLn ""
    putStrLn "where longitude is the longitude (° E) of a geostationary satellite,"
    putStrLn "station is the name of on of the ground stations listed below, and"
    putStrLn "bias (kilometers) is the ranging bias of the ground station."
    putStrLn ""
    mapM_ (putStrLn . fst) stations

main' long' station bias' = do
  [long', station, bias'] <- getArgs
  let long = GeoLongitude $ read long' *~ degree
  let bias = read bias' *~ kilo meter

  let ss = sensitivities long stations
  let s1 = fromMaybe _0 $ join $ lookup station ss
  let longBias = bias * s1  -- This linearization breaks down when SC and GS longitudes coincide.

  putStrLn $ showSC long longBias
  putStrLn $ ""
  putStrLn $ "                    Sensitivity        Est. Bias"
  putStrLn $ "Station         [degE/km]  ([kmE/km])    [km]"
  putStrLn $ "--------------  ---------  ----------  ---------"
  putStrLn $ unlines $ map (showStation longBias) ss
  putStrLn $ ""
  putStrLn $ "Station         Range         Azimuth       Elevation"
  putStrLn $ "--------------  ------------  ------------  -----------"
  putStrLn $ unlines $ map (showAzElRg long) stations

showAzElRg :: GeoLongitude Double -> (String, GeodeticPlace Double) -> String
showAzElRg long (name, gs) = printf "%-14s  %9.3f km  %8.3f deg  %7.3f deg" name (range' gs s/~kilo meter) (azimuth' gs s/~degree) (elevation' gs s/~degree)
  where s = perfectGEO long

