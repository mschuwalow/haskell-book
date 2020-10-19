module Ch24.Exercises where

import Control.Applicative
import Data.Functor
import Text.Trifecta

-- 1.

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Eq, Ord, Show)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer
  = SemVer Major Minor Patch Release Metadata
  deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer ma1 mi1 p1 r1 _) (SemVer ma2 mi2 p2 r2 _)
    | ma1 /= ma2 = compare ma1 ma2
    | mi1 /= mi2 = compare mi1 mi2
    | p1 /= p2 = compare p1 p2
    | null r1 && null r2 = EQ
    | null r1 = GT
    | null r2 = LT
    | otherwise = compare r1 r2

semVerExamples :: [String]
semVerExamples =
  [ "1.0.0-alpha+001",
    "1.0.0+20130313144700",
    "1.0.0-beta+exp.sha.5114f85",
    "1.0.0+21AF26D3-117B344092BD",
    "1.0.0-alpha+001",
    "1.0.0-001+alpha"
  ]

positiveInteger :: Parser Integer
positiveInteger = do
  i <- integer
  if (i >= 0) then return i else fail "most be positive integer"

validString :: Parser String
validString =
  some (oneOf (mconcat [['0' .. '9'], ['a' .. 'z'], ['A' .. 'Z'], ['-']]))

validInteger :: Parser Integer
validInteger =
  try parseInt <|> parseZero
  where
    parseInt = do
      x <- oneOf ['1' .. '9']
      xs <- many digit
      return $ read (x : xs)
    parseZero = char '0' $> 0

numberOrString :: Parser NumberOrString
numberOrString = do
  try (NOSI <$> validInteger <* notFollowedBy validString) <|> (NOSS <$> validString)

parseVersion :: Parser (Integer, Integer, Integer)
parseVersion = do
  major <- validInteger
  _ <- char '.'
  minor <- validInteger
  _ <- char '.'
  patch <- validInteger
  return (major, minor, patch)

parseRelease :: Parser Release
parseRelease = do
  _ <- char '-'
  some (try element <|> numberOrString)
  where
    element = do
      value <- numberOrString
      _ <- char '.'
      return value

parseMeta :: Parser Release
parseMeta = do
  _ <- char '+'
  some (try element <|> numberOrString)
  where
    element = do
      value <- numberOrString
      _ <- char '.'
      return value

parseSemVer :: Parser SemVer
parseSemVer = do
  (maj, min, pat) <- parseVersion
  release <- option [] (try parseRelease)
  meta <- option [] parseMeta
  return $ SemVer maj min pat release meta

-- 2.
parseDigit :: Parser Char
parseDigit = oneOf ['0' .. '9']

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

-- 3.
base10Integer' :: Parser Integer
base10Integer' = (char '-' *> (negate <$> base10Integer)) <|> base10Integer

-- 4.
type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber
  = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  areaCode <- areaCode
  _ <- char '-'
  officeCode <- officeCode
  _ <- char '-'
  lineNumber <- digits 4
  return $ PhoneNumber areaCode officeCode lineNumber

areaCode :: Parser Int
areaCode = do
  code <- digits 3
  let result
        | code < 201 = fail "area code may not start with < 2"
        | otherwise = return code
  result

officeCode :: Parser Int
officeCode = do
  code <- digits 3
  let split = splitNumber code
      result
        | code < 201 = fail "office code may not start with < 2"
        | ((split !! 1)) == 1 && (split !! 2) == 1 = fail "office code may not have one for 2nd and 3rd position"
        | otherwise = return code
  result

digits :: Int -> Parser Int
digits n = read <$> count n digit

splitNumber :: Show a => a -> [Int]
splitNumber n = map (\x -> read [x] :: Int) (show n)
