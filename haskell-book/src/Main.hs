module Main where

data Bool0 = True0 | False0
  deriving Show

f :: Bool0 -> Int
f False0 = 0
f True0 = 1

main :: IO ()
main = print $ f True0
