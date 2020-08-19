module Ch9.Exercises where

import Data.Char

--2.
onlyUpper :: String -> String
onlyUpper = filter isUpper

--3.
capitalizeFirst :: String -> String
capitalizeFirst [] = []
capitalizeFirst (x : xs) = toUpper x : xs

--4.
capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : capitalize xs

--5.
onlyCapitalizeFirst :: String -> Char
onlyCapitalizeFirst = head . capitalize
