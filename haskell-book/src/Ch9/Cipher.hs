module Ch9.Cipher where

import Data.Char

minOrdUpper :: Int
minOrdUpper = ord 'A'

minOrdLower :: Int
minOrdLower = ord 'a'

alphSize :: Int
alphSize = length ['a' .. 'z']

caesarCipher :: Int -> (String -> String, String -> String)
caesarCipher n =
  (ceasar n, ceasar (-n))
  where
      ceasar _ [] = []
      ceasar key (x : xs) = chr (shiftedVal + base) : ceasar key xs
        where
          k = key `mod` alphSize
          isUp = isUpper x
          isLo = isLower x
          base = if isUp then minOrdUpper else minOrdLower
          charVal = ord x - base
          shiftedVal
            | isUp || isLo = (charVal + k) `mod` alphSize
            | otherwise = charVal
