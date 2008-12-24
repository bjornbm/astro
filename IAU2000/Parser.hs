{- 

This program generates a Haskell module with the IAU2000A nutation
series. The data for the series is read from table files downloaded
from [1] and [2]. The IAU2000A series has 1365 term where the first 678
terms are used with luni-solar arguments only and the remaining 687 use
a combination of luni-solar and planetary arguments. Here and in the
generated module the first 678 terms are referred to as /luni-solar/ and
the last 687 as /planetary/ (despite being a combination of luni-solar
and planetary). The luni-solar and planetary terms are separated in [1]
and [2] respectively.

[1] http://maia.usno.navy.mil:80/conv2003/chapter5/tab5.3a.txt
[2] http://maia.usno.navy.mil:80/conv2003/chapter5/tab5.3b.txt
[3] http://aa.usno.navy.mil/publications/docs/Circular_179.pdf

-}

{-
TODO: Instead of converting strings to Dimensionals maybe we should
just write out the strings and the units. The main reason for this
is to maintain full precision in the Tab53 module. The current
version will imply the precision of the Num representation (Double).
-}

import Data.List
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional (Dimensional (Dimensional))
import qualified Prelude


-- | Helper types
type Coeffs a = (Angle a, AngularVelocity a, Angle a)


-- | Parses a line from an @tab5.3a.txt@ file.
parseTab53aLine :: (Floating a, Read a) => String -> ([Dimensionless a], Coeffs a, Coeffs a)
parseTab53aLine ""    = error "Line is empty"
parseTab53aLine ('*':comment) = error $ "Line is a comment:" ++ comment
parseTab53aLine line  = 
  ( [m10,m11,m12,m13,m14] *~~ one
  , (s *~ milli arcsecond, s_dot *~ (milli arcsecond / century), c' *~ milli arcsecond)
  , (c *~ milli arcsecond, c_dot *~ (milli arcsecond / century), s' *~ milli arcsecond)
  ) where
    m10:m11:m12:m13:m14:period:s:s_dot:c:c_dot:c':c_dot':s':s_dot':[] = fmap read (words line)
    century = prefix 36525 day


-- | Parses a line from an @tab5.3b.txt@ file.
parseTab53bLine :: (Floating a, Read a) => String -> ([Dimensionless a], Coeffs a, Coeffs a)
parseTab53bLine line  = 
  ( [m10,m11,m12,m13,m14,m1,m2,m3,m4,m5,m6,m7,m8,m9] *~~ one
  , (s *~ milli arcsecond, 0 *~ (radian / second), c' *~ milli arcsecond)
  , (c *~ milli arcsecond, 0 *~ (radian / second), s' *~ milli arcsecond)
  ) where
    [term,m10,m11,m12,m13,m14,m1,m2,m3,m4,m5,m6,m7,m8,m9,period,s,c',s',c,amp] = fmap read (words line)


-- | Prints a compilable Haskell module for table 5.3a.
printTab53 :: Show a => ([[Dimensionless a]], [Coeffs a], [Coeffs a]) -> String
printTab53 (ms, phis, epss) = unlines
  [ "-- This module was automatically generated."
  , ""
  , "module IAU2000.Tab53 where"
  , ""
  , "import Numeric.Units.Dimensional.Prelude"
  , "import Numeric.Units.Dimensional (Dimensional (Dimensional))"
  , "import qualified Prelude"
  , ""
  , "multipliers :: Fractional a => [[Dimensionless a]]"
  , "multipliers =\n  " ++ intercalate " :\n  " (fmap showMs ms) ++ " :[]"
  , ""
  , "phiCoeffs :: Floating a => [(Angle a, AngularVelocity a, Angle a)]"
  , "phiCoeffs =\n  " ++ intercalate " :\n  " (fmap showCoeffs phis) ++ " :[]"
  , ""
  , "epsCoeffs :: Floating a => [(Angle a, AngularVelocity a, Angle a)]"
  , "epsCoeffs =\n  " ++ intercalate " :\n  " (fmap showCoeffs epss) ++ " :[]"
  , ""
  ]


-- Helper functions for "showing" stuff.

showDim :: Show a => Quantity d a -> String
showDim (Dimensional x) = "Dimensional (" ++ show x ++ ")"

showMs :: Show a => [Dimensionless a] -> String
showMs ms = "[" ++ intercalate ", " (fmap showDim ms) ++ "]"

showCoeffs :: Show a => Coeffs a -> String
showCoeffs (a, a_dot, b) = "(" ++ showDim a ++", "++ showDim a_dot ++ ", " ++ showDim b ++ ")"


-- | Converts the content of an @tab5.3a.txt@ file into compilable Haskell module code.
-- convertTab53a :: String -> [Coeffs a]
convertTab53a = fmap parseTab53aLine . takeWhile (not . comment) . dropWhile comment . lines
  where
    -- | @True@ is the argument is a comment (begins with @'x'@).
    comment :: String -> Bool
    comment ('*':_) = True
    comment _ = False


-- | Converts the content of an @tab5.3a.txt@ file into compilable Haskell module code.
-- convertTab53b :: String -> [Coeffs a]
convertTab53b = fmap parseTab53bLine . reverse . drop 5 . lines


-- | Converts files @IAU2000/tab5.3a.txt@ and @IAU2000/tab5.3b.txt@ into 
-- modules @IAU2000/Tab53a.hs@ and @IAU2000/Tab53b.hs@ respectively.
main = do
  tab53a <- readFile "IAU2000/tab5.3a.txt" >>= return . convertTab53a
  tab53b <- readFile "IAU2000/tab5.3b.txt" >>= return . convertTab53b
  writeFile "IAU2000/Tab53.hs" $ printTab53 $ unzip3 $ tab53a ++ tab53b

