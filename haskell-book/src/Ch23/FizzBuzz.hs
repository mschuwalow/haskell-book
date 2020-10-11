module Ch23.FizzBuzz where

import Control.Monad.Trans.State
import qualified Data.DList as DL

-- simple solution using io
fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

main :: IO ()
main =
  mapM_ (putStrLn . fizzBuzz) [1 .. 100]

-- state using list
fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main' :: IO ()
main' =
  mapM_ putStrLn
    $ reverse
    $ fizzbuzzList [1 .. 100]

-- state using DList
fizzbuzzDList :: [Integer] -> DL.DList String
fizzbuzzDList list =
  execState (mapM_ addResultDList list) DL.empty

addResultDList :: Integer -> State (DL.DList String) ()
addResultDList n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main'' :: IO ()
main'' =
  mapM_ putStrLn $ fizzbuzzDList [1 .. 100]

-- using from to

fizzBuzzFromTo :: Integer -> Integer -> [String]
fizzBuzzFromTo from to =
  let values = enumFromThenTo to (to - 1) from
   in execState (mapM_ addResult values) []

main''' :: IO ()
main''' =
  mapM_ putStrLn $ fizzBuzzFromTo 1 100
