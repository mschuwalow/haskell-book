module Ch8.WordNumber where

import Data.List (intersperse)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . fmap digitToWord . digits

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = digits (n `div` 10) ++ [(mod n 10)]

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "invalid"
