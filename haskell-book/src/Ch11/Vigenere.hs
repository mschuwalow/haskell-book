module Ch11.Vigenere (
  vigenereCipher
) where

import Ch9.Cipher as Cipher
import Data.Char
import Data.List (zip, cycle)

vigenereCipher :: String -> (Cipher.Encode, Cipher.Decode)
vigenereCipher code =
  (encode, decode)
  where
    encode :: Encode
    encode word =
      fmap (uncurry applyCipher) codes
      where
        codes = (makeKey word code)
    decode word =
      fmap (uncurry applyCipher) codes
      where
        codes = fmap invertCode (makeKey word code)
        invertCode = fmap (fmap (negate))
    applyCipher :: Char -> Maybe Int -> Char
    applyCipher c Nothing  = c
    applyCipher c (Just n) = head . (fst . Cipher.caesarCipher $ n) $ [c]


makeKey :: String -> String -> [(Char, Maybe Int)]
makeKey message keyword = go message keyCycle
  where
    keyCycle = cycle keyword
    go [] _  = []
    go xs [] = zip xs (repeat Nothing)
    go (x:xs) (y:ys)
      | x == ' '  = (' ', Nothing) : go xs (y:ys)
      | otherwise = (x, Just (Cipher.charVal y)) : go xs ys
