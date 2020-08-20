module Ch9.Cipher where

import Data.Char
import GHC.Stack (HasCallStack)

type Encode = String -> String

type Decode = String -> String

encodeChar :: Char -> Bool
encodeChar x =
  isAsciiLower x || isAsciiUpper x

alphSize :: Int
alphSize = length ['a' .. 'z']

base :: HasCallStack => Char -> Int
base x
  | isAsciiLower x = ord 'a'
  | isAsciiUpper x = ord 'A'
  | otherwise = error $ "can only get base for ascii chars, input was `" ++ [x] ++ "`"

charVal :: Char -> Int
charVal x = ord x - base x

caesarCipher :: Int -> (Encode, Decode)
caesarCipher n =
  (ceasar n, ceasar (- n))
  where
    ceasar _ [] = []
    ceasar key (x : xs)
      | encodeChar x = chr ((((charVal x) + key) `mod` alphSize) + base x) : ceasar key xs
      | otherwise = x : ceasar key xs
