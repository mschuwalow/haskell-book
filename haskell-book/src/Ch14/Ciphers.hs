module Ch14.Ciphers where

import Ch11.Vigenere (vigenereCipher)
import Ch9.Cipher (caesarCipher)
import Test.QuickCheck

cipherInvertible :: Eq a => (p -> (a -> b, b -> a)) -> p -> a -> Bool
cipherInvertible f c x =
  (decode . encode $ x) == x
  where
    (encode, decode) = f c

runQc :: IO ()
runQc = do
  quickCheck (cipherInvertible caesarCipher)
  quickCheck (cipherInvertible vigenereCipher)
