module Ch12.StringProcessing where

import Data.Char (toLower)
import Data.List (intersperse)

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe =
  concat
    . intersperse " "
    . fmap (maybe "a" id . notThe)
    . words

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (x : _)
  | isVowel x = True
  | otherwise = False

isVowel :: Char -> Bool
isVowel x = case toLower x of
  'a' -> True
  'e' -> True
  'i' -> True
  'o' -> True
  'u' -> True
  _ -> False

countVowels :: String -> Int
countVowels =
  length
    . filter isVowel

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel =
  go 0 . words
  where
    go :: Integer -> [String] -> Integer
    go n (x1 : x2 : xs)
      | x1 == "the" && startsWithVowel x2 = go (n + 1) xs
      | otherwise = go n xs
    go n _ = n
