module Ch9.Ciphers where

import Data.Char

type Encrypt = (String -> String)

type Decrypt = (String -> String)

myCiphers :: Int -> (Encrypt, Decrypt)
myCiphers shiftBy =
  if (shiftBy < 0)
    then error "must be positive number"
    else (encrypt, decrypt)
  where
    encrypt = \str -> map (\c -> shift c shiftBy) str
    decrypt = \str -> map (\c -> shift c (mod (26 - shiftBy) 26)) str

shiftUpper :: Char -> Int -> Char
shiftUpper c n = chr ((rem ((ord c - 65) + n) 26) + 65)

shiftLower :: Char -> Int -> Char
shiftLower c n = chr ((rem ((ord c - 97) + n) 26) + 97)

shift :: Char -> Int -> Char
shift c n
  | isUpper c = shiftUpper c n
  | isLower c = shiftLower c n
  | otherwise = c -- remian same
