module Ch13.Ciphers where

import Ch9.Cipher     (caesarCipher)
import Ch11.Vigenere  (vigenereCipher)
import Control.Monad  (forever)
import System.IO      (BufferMode(NoBuffering),
                       hSetBuffering,
                       stdout)
import Text.Read      (readMaybe)

caesarEncode :: IO ()
caesarEncode =
  makeInteractive (fst . caesarCipher)

caesarDecode :: IO ()
caesarDecode =
  makeInteractive (snd . caesarCipher)

vigenereEncode :: IO ()
vigenereEncode =
  makeInteractive (fst . vigenereCipher)

vigenereDecode :: IO ()
vigenereDecode =
  makeInteractive (snd . vigenereCipher)

makeInteractive :: (Read n) => (n -> (String -> String)) -> IO ()
makeInteractive f = do
  hSetBuffering stdout NoBuffering
  code <- readCode
  let transform = f code
  forever $ do
    putStr "==> "
    input <- getLine
    putStrLn $ "<== " ++ transform input
  where
    readCode = do
      putStr "Please enter the code: "
      raw <- getLine
      let parsed = readMaybe raw
      maybe (putStrLn "Invalid code!" >> readCode) return parsed
