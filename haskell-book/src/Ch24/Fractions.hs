module Ch24.Fractions where

import Control.Applicative
import Data.Ratio (Ratio, (%))
import Text.Trifecta

badFraction :: [Char]
badFraction = "1/0"

alsoBad :: [Char]
alsoBad = "10"

shouldWork :: [Char]
shouldWork = "1/2"

shouldAlsoWork :: [Char]
shouldAlsoWork = "2/1"

parseFraction :: Parser (Ratio Integer)
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseFractionOrDecimal :: Parser (Either Rational Integer)
parseFractionOrDecimal =
  Left <$> (try parseFraction) <|> Right <$> decimal

parseIntEOF :: Parser Integer
parseIntEOF = do
  n <- integer
  eof
  return n

main :: IO ()
main = do
  let parseFraction' =
        parseString parseFraction mempty
  print $ parseFraction' badFraction
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork
  print $ parseFraction' alsoBad
