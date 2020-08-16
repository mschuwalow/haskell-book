module Ch13.Palindrome where

import Control.Monad
import Data.Char (isLower, isUpper, toLower)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome line1) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"

isPalindrome :: String -> Bool
isPalindrome word =
  transformed == (reverse transformed)
  where
    transformed = fmap toLower . stripSymbols $ word

stripSymbols :: String -> String
stripSymbols =
  filter (\c -> isLower c || isUpper c)
