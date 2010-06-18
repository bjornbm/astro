{-# LANGUAGE NamedFieldPuns #-}

import Astro.Celestrak
import Data.Time
import Text.Parsec
import Text.Parsec.String
--import Text.Parsec.ByteString.Lazy
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Numeric.Units.Dimensional.Prelude
import qualified Prelude

lexer = P.makeTokenParser haskellDef    
      
integer = P.integer lexer
float   = P.float   lexer

float' = do
  many space
  f <- sign
  float >>= return . f
  where sign = choice [char '-' >> return Prelude.negate, char '+' >> return id, return id]

mjd :: Parser Day
mjd = integer >>= return . ModifiedJulianDay

arcsec :: Parser (Angle Double)
arcsec = float' >>= return . (*~arcsecond)

secs :: Parser (Time Double)
secs = float' >>= return . (*~second)


{-
# FORMAT(I4,I3,I3,I6,2F10.6,2F11.7,4F10.6,I4)
# ----------------------------------------------------------------------------------------------------
#   Date    MJD      x         y       UT1-UTC      LOD       dPsi    dEpsilon     dX        dY    DAT
# (0h UTC)           "         "          s          s          "        "          "         "     s 
# ----------------------------------------------------------------------------------------------------
# yy dd mm nnnnn +n.nnnnnn +n.nnnnnn +n.nnnnnnn +n.nnnnnnn +n.nnnnnn +n.nnnnnn +n.nnnnnn +n.nnnnnn nnn
# ----------------------------------------------------------------------------------------------------
#
NUM_OBSERVED_POINTS 2116
BEGIN OBSERVED
2003 01 01 52640 -0.088474  0.188235 -0.2894287  0.0004278 -0.055412 -0.000565 -0.000054  0.000103  32
-}

line :: Parser (Day, EOPData Double)
line = do
  integer >> integer >> integer
  d           <- mjd
  x           <- arcsec
  y           <- arcsec
  ut1MinusUTC <- secs
  lod         <- secs
  dPsi        <- arcsec
  dEpsilon    <- arcsec
  dX          <- arcsec
  dY          <- arcsec
  deltaAT     <- integer  -- Seems to consume trailing newline!?
  --newline
  return (d, EOPData { x, y, ut1MinusUTC, lod, dPsi, dEpsilon, dX, dY, deltaAT })

--parseEOPFile :: String -> EOPList a
parseEOPFile = parseFromFile parser

parser :: Parser (EOPList Double)
parser = between (string "BEGIN PREDICTED" >> newline) (string "END PREDICTED" >> newline) (many line)

testLine = "2003 01 01 52640 -0.088474  0.188235 -0.2894287  0.0004278 -0.055412 -0.000565 -0.000054  0.000103  32"
test = parse line "bub" testLine
test2 = parseEOPFile "tmp.bb"








