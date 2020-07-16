module Ch10.DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  foldr (\next acc -> f next ++ acc) []
  where
    f (DbDate time) = [time]
    f _ = []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  foldr (\next acc -> f next ++ acc) []
  where
    f (DbNumber time) = [time]
    f _ = []

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent =
  foldr f (UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 1))
  where
    f (DbDate x) y = max x y
    f _ x = x

sumDb :: [DatabaseItem] -> Integer
sumDb =
  foldr f 0
  where
    f (DbNumber x) y = x + y
    f _ x = x

avgDb :: [DatabaseItem] -> Double
avgDb db =
  (fromIntegral . sum $ nums) / (fromIntegral . length $ nums)
  where
    nums = filterDbNumber db
