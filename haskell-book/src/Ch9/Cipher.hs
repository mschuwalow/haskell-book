module Ch9.Cipher where

import Data.Char

type Encode = String -> String

type Decode = String -> String

base :: Char -> Int
base x =
  if isLower x
    then ord 'a'
    else
      if isUpper x
        then ord 'A'
        else 0

encodeChar :: Char -> Bool
encodeChar x =
  isLower x || isUpper x

alphSize :: Int
alphSize = length ['a' .. 'z']

charVal :: Char -> Int
charVal x = ord x - base x

caesarCipher :: Int -> (Encode, Decode)
caesarCipher n =
  (ceasar n, ceasar (- n))
  where
    ceasar _ [] = []
    ceasar key (x : xs) =
      chr (shiftedVal + base x) : ceasar key xs
      where
        shiftedVal
          | encodeChar x = ((charVal x) + key) `mod` alphSize
          | otherwise = charVal x
