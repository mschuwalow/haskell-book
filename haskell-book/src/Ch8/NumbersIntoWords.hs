module Ch8.NumbersIntoWords where

import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n =
  case n of
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    0 -> "zero"
    _ -> undefined

digits :: Int -> [Int]
digits =
  reverse . go . abs
  where
    go n
      | n < 10 = [n]
      | otherwise = mod n 10 : (go $ div n 10)

wordNumber :: Int -> String
wordNumber =
  intercalate "-" . map digitToWord . digits
