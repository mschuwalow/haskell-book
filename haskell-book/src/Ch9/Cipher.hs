module Ch9.Cipher where

import Data.Char

minOrdUpper :: Int
minOrdUpper = ord 'A'

minOrdLower :: Int
minOrdLower = ord 'a'

base :: Char -> Int
base x =
  if isLower x
  then minOrdUpper
  else minOrdLower

encodeChar :: Char -> Bool
encodeChar x =
  isLower x || isUpper x

alphSize :: Int
alphSize = length ['a' .. 'z']

charVal :: Char -> Int
charVal x = ord x - base x

caesarCipher :: Int -> (String -> String, String -> String)
caesarCipher n =
  (ceasar n, ceasar (-n))
  where
      ceasar _ [] = []
      ceasar key (x : xs) =
        chr (shiftedVal + base x) : ceasar key xs
        where
          key' = key `mod` alphSize
          shiftedVal
            | encodeChar x = ((charVal x) + key') `mod` alphSize
            | otherwise    = charVal x
