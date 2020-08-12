module Ch12.ValidateTheWord where

import Data.Char (toLower)
import Ch12.StringProcessing (isVowel)

isConsonant :: Char -> Bool
isConsonant x = case toLower x of
  'b' -> True
  'c' -> True
  'd' -> True
  'f' -> True
  'g' -> True
  'h' -> True
  'j' -> True
  'k' -> True
  'l' -> True
  'm' -> True
  'n' -> True
  'p' -> True
  'q' -> True
  'r' -> True
  's' -> True
  't' -> True
  'v' -> True
  'w' -> True
  'x' -> True
  'y' -> True
  'z' -> True
  _  -> False

newtype Word' =
  Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s =
  if (nVowels <= nConsonants)
  then Just $ Word' s
  else Nothing
  where
    nVowels =
      length
      . filter isVowel
      $ s
    nConsonants =
      length
      . filter isConsonant
      $ s
