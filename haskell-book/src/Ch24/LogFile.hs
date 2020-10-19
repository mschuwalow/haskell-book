{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ch24.LogFile where

import Control.Applicative
import Control.Monad (void)
import Data.Function ((&))
import Data.List
import Test.Hspec
import Test.QuickCheck hiding (Result, Success)
import Text.Printf (printf)
import Text.RawString.QQ
import Text.Trifecta

type Year = Integer

type Month = Integer

type Day = Integer

type Hour = Integer

type Minute = Integer

type Description = String

data Date = Date Year Month Day
  deriving (Eq, Ord)

instance Show Date where
  show (Date year month day) =
    printf "%04d" year ++ "-"
      ++ printf "%02d" month
      ++ "-"
      ++ printf "%02d" day

data Time = Time Hour Minute
  deriving (Eq, Ord)

instance Show Time where
  show (Time hour min) = printf "%02d" hour ++ ":" ++ printf "%02d" min

data Activity
  = Activity Time Description
  deriving (Eq)

instance Show Activity where
  show (Activity time desc) = show time ++ " " ++ desc

data DailyLog
  = DailyLog Date [Activity]
  deriving (Eq)

instance Show DailyLog where
  show (DailyLog date activities) =
    unlines $
      ("# " ++ show date)
        : (map show activities)

---

skipComment :: Parser ()
skipComment = char '-' *> char '-' *> skipRestOfLine

skipRestOfLine :: Parser ()
skipRestOfLine = void $ manyTill anyChar ((void newline) <|> eof)

skipSpaceAndComments :: Parser ()
skipSpaceAndComments = skipMany (skipComment <|> skipSome space)

parseDate :: Parser Date
parseDate = Date <$> (decimal <* char '-') <*> (decimal <* char '-') <*> decimal

parseDateLine :: Parser Date
parseDateLine = char '#' *> char ' ' *> parseDate <* skipRestOfLine

parseTime :: Parser Time
parseTime = Time <$> (decimal <* char ':') <*> decimal

parseDescription :: Parser Description
parseDescription = manyTill anyChar (try skipComment <|> (void newline) <|> eof)

parseActivity :: Parser Activity
parseActivity = Activity <$> parseTime <*> (char ' ' *> parseDescription)

parseDay :: Parser DailyLog
parseDay = do
  date <- parseDateLine
  activities <- some parseActivity
  skipSpaceAndComments
  return $ DailyLog date activities

parseLog :: Parser [DailyLog]
parseLog = skipSpaceAndComments *> some parseDay

---
genPositiveInteger :: Gen Integer
genPositiveInteger = (getPositive <$> arbitrary)

genStringNoComments :: Gen String
genStringNoComments =
  arbitrary
    & (`suchThat` (not . isInfixOf "--"))
    . (`suchThat` (not . isInfixOf "\n"))

instance Arbitrary Date where
  arbitrary = liftA3 Date genPositiveInteger genPositiveInteger genPositiveInteger

instance Arbitrary Time where
  arbitrary = liftA2 Time genPositiveInteger genPositiveInteger

instance Arbitrary Activity where
  arbitrary = liftA2 Activity arbitrary genStringNoComments

instance Arbitrary DailyLog where
  arbitrary = liftA2 DailyLog arbitrary (getNonEmpty <$> arbitrary)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

example :: String
example =
  [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

main :: IO ()
main = hspec $ do
  describe "parseLog" $ do
    it "should parse generated logs" $ property $
      \(log :: DailyLog) ->
        (maybeSuccess . parseString parseLog mempty . show $ log) == (Just [log])
