{- 

This program generates a Haskell module with the IAU2000A nutation
series. The data for the series is read from table files downloaded
from [1] and [2]. The IAU2000A series has 1365 term where the first 678
terms are used with luni-solar arguments only and the remaining 687 use
a combination of luni-solar and planetary arguments. Here and in the
generated module the first 678 terms are referred to as /luni-solar/ and
the last 687 as /planetary/ (despite being a combination of luni-solar
and planetary). The luni-solar and planetary terms are separated in [1]
and [2] respectively. This module combines all terms in a single table
analogously to that published in [3].

[1] http://maia.usno.navy.mil:80/conv2003/chapter5/tab5.3a.txt
[2] http://maia.usno.navy.mil:80/conv2003/chapter5/tab5.3b.txt
[3] http://aa.usno.navy.mil/publications/docs/Circular_179.pdf

-}

import Data.List


-- Type synonyms for function signatures.
type Multipliers = [String]
type Coeffs = (String, String, String)


-- Line parsing
-- ------------
-- | Parses a line from the @tab5.3a.txt@ file.
parseTab53aLine :: String -> (Multipliers, Coeffs, Coeffs)
parseTab53aLine line  = ([m10,m11,m12,m13,m14] , (s, s_dot, c') , (c, c_dot, s'))
  where [m10,m11,m12,m13,m14,period,s,s_dot,c,c_dot,c',c_dot',s',s_dot'] = words line

-- | Parses a line from the @tab5.3b.txt@ file.
parseTab53bLine :: String -> (Multipliers, Coeffs, Coeffs)
parseTab53bLine line = ([m10,m11,m12,m13,m14,m1,m2,m3,m4,m5,m6,m7,m8,m9] , (s, "0", c') , (c, "0", s'))
  where [term,m10,m11,m12,m13,m14,m1,m2,m3,m4,m5,m6,m7,m8,m9,period,s,c',s',c,amp] = words line


-- Table parsing
-- -------------
-- | Converts the applicable content of an @tab5.3a.txt@ file into
-- compilable Haskell module code.
convertTab53a :: String -> [(Multipliers, Coeffs, Coeffs)]
convertTab53a = fmap parseTab53aLine . takeWhile (not . comment) . dropWhile comment . lines
  where comment = ('*'==) . head

-- | Converts the content of an @tab5.3a.txt@ file into compilable Haskell module code.
-- convertTab53b :: String -> [Coeffs a]
convertTab53b :: String -> [(Multipliers, Coeffs, Coeffs)]
convertTab53b = fmap parseTab53bLine . reverse . drop 5 . lines


-- Module printing
-- ---------------
-- | Prints a compilable Haskell module for table 5.3.
printTable53 :: ([Multipliers], [Coeffs], [Coeffs]) -> String
printTable53 (ms, phis, epss) = unlines
  [ "{-"
  , ""
  , "This module was automatically generated from [1] and [2]."
  , "It provides the entire IAU 2000A nutation series on list form."
  , ""
  , "[1] http://maia.usno.navy.mil:80/conv2003/chapter5/tab5.3a.txt"
  , "[2] http://maia.usno.navy.mil:80/conv2003/chapter5/tab5.3b.txt"
  , "[3] http://aa.usno.navy.mil/publications/docs/Circular_179.pdf"
  , ""
  , "-}"
  , ""
  , "module IAU2000.Table53 (multipliers, phiCoeffs, epsCoeffs) where"
  , ""
  , "import Astro.Time (century)"
  , "import Numeric.Units.Dimensional.Prelude"
  , "import Numeric.Units.Dimensional (Dimensional (Dimensional))"
  , "import qualified Prelude"
  , ""
  , "mas :: Floating a => Unit DPlaneAngle a"
  , "mas = milli arcsecond"
  , ""
  , "-- | Returns the series of fundamental argument multipliers. The multipliers"
  , "-- are on the order [m10,m11,m12,m13,m14,m1,m2,m3,m4,m5,m6,m7,m8,m9] where"
  , "-- the indices correspond to those of the fundamental arguments on page 46"
  , "-- of [3]. For the luni-solar terms (the first 678 terms in the series) only"
  , "-- m10 through m14 are provided."
  , "multipliers :: Fractional a => [[Dimensionless a]]"
  , "multipliers =\n  " ++ showList' (fmap showMultipliers ms)
  , ""
  , "-- | Sine and cosine coefficients @(s, s_dot, c')@ for evaluating the"
  , "-- nutation in longitude (Delta-phi)."
  , "phiCoeffs :: Floating a => [(Angle a, AngularVelocity a, Angle a)]"
  , "phiCoeffs =\n  " ++ showList' (fmap showCoeffs phis)
  , ""
  , "-- | Cosine and sine coefficients @(c, c_dot, s')@ for evaluating the"
  , "-- nutation in obliquity (Delta-epsilon)."
  , "epsCoeffs :: Floating a => [(Angle a, AngularVelocity a, Angle a)]"
  , "epsCoeffs =\n  " ++ showList' (fmap showCoeffs epss)
  , ""
  ]

-- Helper functions for "showing" stuff.

showMultipliers :: Multipliers -> String
showMultipliers ms = "([" ++ intercalate ", " ms ++ "] *~~ one)"

showCoeffs :: Coeffs -> String
showCoeffs (a, a_dot, b) = "((" ++ a  ++") *~ mas, ("++ a_dot ++ ") *~ (mas / century), (" ++ b ++ ") *~ mas)"

showList' :: [String] -> String
showList' ss = intercalate " :\n  " ss ++ " :[]"


-- | Converts files @IAU2000/tab5.3a.txt@ and @IAU2000/tab5.3b.txt@ into 
-- module "IAU2000.Table53" (file @IAU2000/Table53.hs@).
main = do
  tab53a <- readFile "IAU2000/tab5.3a.txt"
  tab53b <- readFile "IAU2000/tab5.3b.txt"
  writeFile "IAU2000/Table53.hs" $ printTable53 $ unzip3 $ convertTab53a tab53a ++ convertTab53b tab53b

