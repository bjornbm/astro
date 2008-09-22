{- 

This program generates Haskell modules with the IAU2000A nutation
series. The data for the series is read from table files downloaded
from [1] and [2]. The IAU2000A series has 1365 term where the first
678 terms are used with luni-solar arguments only and the remaining
687 use a combination of luni-solar and planetary arguments. Here
and in the generated modules the first 678 terms are referred to
as /luni-solar/ and the last 687 as /planetary/ (despite being a
combination of luni-solar and planetary). The luni-solar and planetary
terms are separated in [1] and [2] respectively. We maintain this
\"optimization\" and generate separate modules with separate
datastructures omitting unnecessary coefficients.

[1] http://maia.usno.navy.mil:80/conv2003/chapter5/tab5.3a.txt
[2] http://maia.usno.navy.mil:80/conv2003/chapter5/tab5.3b.txt
[3] http://aa.usno.navy.mil/publications/docs/Circular_179.pdf

-}

import Data.List
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional (Dimensional (Dimensional))
import qualified Prelude


-- | Helper types
type LuniSolarCoeffs a = (Angle a, AngularVelocity a, Angle a)
type PlanetaryCoeffs a = (Angle a, Angle a)


-- | Parses a line from an @tab5.3a.txt@ file.
parseTab53aLine :: (Floating a, Read a) => String -> ([Dimensionless a], LuniSolarCoeffs a, LuniSolarCoeffs a)
parseTab53aLine ""    = error "Line is empty"
parseTab53aLine ('*':comment) = error $ "Line is a comment:" ++ comment
parseTab53aLine line  = 
  ( [m10,m11,m12,m13,m14] *~~ one
  , (s *~ milli arcsecond, s_dot *~ (milli arcsecond / century), c' *~ milli arcsecond)
  , (c *~ milli arcsecond, c_dot *~ (milli arcsecond / century), s' *~ milli arcsecond)
  ) where
    m10:m11:m12:m13:m14:period:s:s_dot:c:c_dot:c':c_dot':s':s_dot':[] = fmap read (words line)


-- | Parses a line from an @tab5.3b.txt@ file.
parseTab53bLine :: (Floating a, Read a) => String -> ([Dimensionless a], PlanetaryCoeffs a, PlanetaryCoeffs a)
parseTab53bLine line  = 
  ( [m10,m11,m12,m13,m14,m1,m2,m3,m4,m5,m6,m7,m8,m9] *~~ one
  , (s *~ milli arcsecond, c' *~ milli arcsecond)
  , (c *~ milli arcsecond, s' *~ milli arcsecond)
  ) where
    [term,m10,m11,m12,m13,m14,m1,m2,m3,m4,m5,m6,m7,m8,m9,period,s,c',s',c,amp] = fmap read (words line)


-- | Prints a compilable Haskell module for table 5.3a.
printTab53a :: Show a => ([[Dimensionless a]], [LuniSolarCoeffs a], [LuniSolarCoeffs a]) -> String
printTab53a (ms, phis, epss) = unlines
  [ "-- This module was automatically generated."
  , ""
  , "module IAU2000.Tab53a where"
  , ""
  , "import Numeric.Units.Dimensional.Prelude"
  , "import Numeric.Units.Dimensional (Dimensional (Dimensional))"
  , "import qualified Prelude"
  , ""
  , "luniSolarMultipliers :: Fractional a => [[Dimensionless a]]"
  , "luniSolarMultipliers = " ++ intercalate " :\n  " (fmap showMs ms) ++ " :[]"
  , ""
  , "luniSolarPhiCoeffs :: Floating a => [(Angle a, AngularVelocity a, Angle a)]"
  , "luniSolarPhiCoeffs = " ++ intercalate " :\n  " (fmap showLSCoeffs phis) ++ " :[]"
  , ""
  , "luniSolarEpsCoeffs :: Floating a => [(Angle a, AngularVelocity a, Angle a)]"
  , "luniSolarEpsCoeffs = " ++ intercalate " :\n  " (fmap showLSCoeffs epss) ++ " :[]"
  , ""
  ]


-- | Prints a compilable Haskell module for table 5.3b.
printTab53b :: Show a => ([[Dimensionless a]], [PlanetaryCoeffs a], [PlanetaryCoeffs a]) -> String
printTab53b (ms, phis, epss) = unlines
  [ "-- This module was automatically generated."
  , ""
  , "module IAU2000.Tab53b where"
  , ""
  , "import Numeric.Units.Dimensional.Prelude"
  , "import Numeric.Units.Dimensional (Dimensional (Dimensional))"
  , "import qualified Prelude"
  , ""
  , "planetaryMultipliers :: Fractional a => [[Dimensionless a]]"
  , "planetaryMultipliers = " ++ intercalate " :\n  " (fmap showMs ms) ++ " :[]"
  , ""
  , "planetaryPhiCoeffs :: Floating a => [(Angle a, Angle a)]"
  , "planetaryPhiCoeffs = " ++ intercalate " :\n  " (fmap showPCoeffs phis) ++ " :[]"
  , ""
  , "planetaryEpsCoeffs :: Floating a => [(Angle a, Angle a)]"
  , "planetaryEpsCoeffs = " ++ intercalate " :\n  " (fmap showPCoeffs epss) ++ " :[]"
  , ""
  ]


-- Helper functions for "showing" stuff.

showDim :: Show a => Quantity d a -> String
showDim (Dimensional x) = "Dimensional (" ++ show x ++ ")"

showMs :: Show a => [Dimensionless a] -> String
showMs ms = "[" ++ intercalate ", " (fmap showDim ms) ++ "]"

showLSCoeffs :: Show a => LuniSolarCoeffs a -> String
showLSCoeffs (a, a_dot, b) = "(" ++ showDim a ++", "++ showDim a_dot ++ ", " ++ showDim b ++ ")"

showPCoeffs :: Show a => PlanetaryCoeffs a -> String
showPCoeffs  (a, b) = "(" ++ showDim a ++", " ++ showDim b ++ ")"


-- | Converts the content of an @tab5.3a.txt@ file into compilable Haskell module code.
convertTab53a :: String -> String
convertTab53a = printTab53a . unzip3 . fmap parseTab53aLine . takeWhile (not . comment) . dropWhile comment . lines
  where
    -- | @True@ is the argument is a comment (begins with @'x'@).
    comment :: String -> Bool
    comment ('*':_) = True
    comment _ = False


-- | Converts the content of an @tab5.3a.txt@ file into compilable Haskell module code.
convertTab53b :: String -> String
convertTab53b = printTab53b . unzip3 . fmap parseTab53bLine . drop 5 . lines


-- | Converts files @IAU2000/tab5.3a.txt@ and @IAU2000/tab5.3b.txt@ into 
-- modules @IAU2000/Tab53a.hs@ and @IAU2000/Tab53b.hs@ respectively.
main = do
  readFile "IAU2000/tab5.3a.txt" >>= writeFile "IAU2000/Tab53a.hs" . convertTab53a
  readFile "IAU2000/tab5.3b.txt" >>= writeFile "IAU2000/Tab53b.hs" . convertTab53b

