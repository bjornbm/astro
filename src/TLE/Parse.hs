{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TLE.Parse where

import Numeric.Units.Dimensional.Prelude hiding (name, take, takeWhile)
import Numeric.Units.Dimensional.NonSI (revolution)
import Data.Time
import Data.Char (isAlpha, isLetter, isDigit, digitToInt)
import qualified Data.Text as T hiding (take, takeWhile, count)
import Data.Attoparsec.Text
import qualified Prelude
import Control.Monad (sequence)
import Control.Applicative
import Data.Monoid (mappend, mconcat)
import TLE



parseNoradID = take 5 >>= parseRight decimal
parseClassification =  (const Unclassified <$> char 'U')
                   <|> (const Secret       <$> char 'S')
                   <|> (Classified <$> anyChar)

parseIntlDesignator = IntlDesignator
    <$> (take 2 >>= parseRight parseYear)
    <*> (take 3 >>= parseRight decimal)
    <*> (take 3 >>= parseLeft (takeWhile1 isAlpha))

parseYear :: Parser Integer
parseYear = ifThenElseF (<57) (Prelude.+2000) (Prelude.+1900) <$> decimal
  where ifThenElseF p t f x = if p x then t x else f x

parseEpoch = do
    y <- take 2 >>= parseRight parseYear
    d <- take 12 >>= parseRight rational
    return $ addUTCTime (realToFrac $ (d*~day - 1*~day) /~ second)
                        (UTCTime (fromGregorian y 1 1) 0)
           -- TODO upgrade attoparsec and remove `realToFrac`.

-- | Massage the sign of numbers on the format +.NNNNNNNN or +NNNNN-N.
sign =  (char '-' >> return "-0.")
    <|> (char '+' <|> char ' ' >> return "0.")

-- | Massage the exponent of numbers on the format +NNNNN-N.
expo =  (char '-' >> return "e-")
    <|> (char '+' >> return "e")

parseDMdt = parseLeft' $ mappend <$> sign <*> (char '.' >> take 8)
                     >>= parseLeft rational
                     >>= return . \x -> x *~ (revolution * day^neg2) * _2

parseD2Mdt2 = parseLeft' $ mconcat <$> sequence [sign, take 5, expo, take 1]
                       >>= parseLeft rational
                       >>= return . \x -> x *~ (revolution * day^neg3) * _6

parseBStar = parseLeft' $ mconcat <$> sequence [sign, take 5, expo, take 1]
                      >>= parseLeft rational
                      >>= return . \x -> x *~ one / r_Earth
  where r_Earth = 6431 *~ (kilo meter)

degreeP = (*~degree) <$> rational

eccentricityP = mappend "0." <$> take 7
            >>= parseLeft rational
            >>= return . (*~ one)

meanMotionP = (*~(revolution/day)) <$> rational

parseEphemerisType = do
    x <- digitToInt <$> digit
    return $ case x of
      0 -> SGP4_SDP4  -- 0
      1 -> SGP        -- 1
      2 -> SGP4       -- 2
      3 -> SDP4       -- 3
      4 -> SGP8       -- 4
      5 -> SDP8       -- 5
      _ -> EphemerisType x

--trace' :: Show a => a -> a
--trace' x = traceShow x x

parseTLE :: Parser (TLE Double)
parseTLE = do
    name <- T.strip <$> takeTill isEndOfLine
    endOfLine
    -- Line 1
    char '1'
    space
    noradID <- parseNoradID
    classification <- parseClassification
    space
    intlDesignator <- parseIntlDesignator
    space
    epoch <- parseEpoch
    space
    dMdt <- takeParseNote "dM/dt" 10 parseDMdt
    space
    d2Mdt2 <- takeParseNote "d^2M/dt^2" 8 parseD2Mdt2
    space
    bStar <- takeParseNote "B*" 8 parseBStar
    space
    ephemerisType <- parseEphemerisType
    space
    elementNo <- takeParseNote "element number" 4 $ parseRight' decimal
    digit
    endOfLine
    char '2'
    space
    noradID2 <- parseNoradID
    if noradID2 /= noradID then fail "Inconsistent NORAD IDs on lines 1 and 2"
                           else return ()
    space
    inclination <- takeParseNote "inclination" 8 $ parseRight' degreeP
    space
    raan <- takeParseNote "RAAN" 8 $ parseRight' degreeP
    space
    eccentricity <- takeParseNote "eccentricity" 7 $ parseLeft' eccentricityP
    space
    aop <- takeParseNote "AoP" 8 $ parseRight' degreeP
    space
    meanAnomaly <- takeParseNote "mean anomaly" 8 $ parseRight' degreeP
    space
    meanMotion <- takeParseNote "mean motion" 11 $ parseRight' meanMotionP
    revs <- takeParseNote "revolutions" 5 $ parseRight' decimal
    digit
    return $ TLE { name
                 , noradID
                 , classification
                 , intlDesignator
                 , epoch
                 , dMdt
                 , d2Mdt2
                 , bStar
                 , ephemerisType
                 , elementNo
                 , inclination
                 , raan
                 , eccentricity
                 , aop
                 , meanAnomaly
                 , meanMotion
                 , revs
                 }


checkValue :: Parser Int
checkValue =  (char '-' >> return 1)
        <|> (digitToInt <$> digit)
        <|> (anyChar >> return 0)

--checkSum :: Parser Int
--checkSum = (`mod` 10) . Prelude.sum <$> (many1 checkValue)
checkSum :: Parser Bool
checkSum = (\xs -> Prelude.sum (init xs) `mod` 10 == last xs) <$> (many1 checkValue)


parseRight :: Parser a -> T.Text -> Parser a
parseRight = pparse . parseRight'
-- Parse a right justified field.
parseRight' p = do
    skipSpace
    res <- p
    endOfInput
    return res

parseLeft = pparse . parseLeft'
-- Parse a Left justified field.
parseLeft' p = do
    res <- p
    skipSpace
    endOfInput
    return res



pparse :: Parser a -> T.Text -> Parser a
pparse p t = case parseOnly p t of
                  Left  err -> fail err
                  Right res -> return res

-- Apply a parser to a specified number of characters. Useful for
-- parsing fixed width fields.
takeParse :: Int -> Parser a -> Parser a
takeParse n p = take n >>= pparse p

takeParseNote :: String -> Int -> Parser a -> Parser a
takeParseNote desc n p = do
    t <- take n
    pparse p t <?> "Bad " ++ desc ++ ": \"" ++ T.unpack t ++ "\""

main = interact
    ( unlines
    . map show
    . map mangle
    . blubb
    . map T.init
    . T.lines
    . T.pack
    )

data RawTLE = RawTLE T.Text T.Text T.Text
            | BadTLE 
blubb :: [T.Text] -> [RawTLE]
blubb (l0:l1:l2:ls) = RawTLE l0 l1 l2:blubb ls
blubb _ = []  -- Silently throws away any non-triple at the end.

mangle :: RawTLE -> Either String (TLE Double)
mangle r@(RawTLE l0 l1 l2) = do
    ok1 <- parseOnly checkSum l1
    if ok1 then return () else Left $ badTLE "Bad checksum in line 1" r
    ok2 <- parseOnly checkSum l2
    if ok2 then return () else Left $ badTLE "Bad checksum in line 2" r
    parseOnly parseTLE $ T.unlines [l0,l1,l2]

badTLE err (RawTLE l0 l1 l2) = err ++ " of:\n"
        ++ "Name:    " ++ T.unpack l0 ++ "\n"
        ++ "Line 1:  " ++ T.unpack l1 ++ "\n"
        ++ "Line 2:  " ++ T.unpack l2 ++ "\n"
