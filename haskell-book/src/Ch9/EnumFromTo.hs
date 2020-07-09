module Ch9.EnumFromTo where

eftBool :: Bool -> Bool -> [Bool]
eftBool = eftBool

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftOrd

eftInt :: Int -> Int -> [Int]
eftInt = eftEnum

eftChar :: Char -> Char -> [Char]
eftChar = eftEnum

eftEnum :: (Ord a, Enum a) => a -> a -> [a]
eftEnum x y
  | x > y     = []
  | x == y    = [x]
  | otherwise = x : eftEnum (succ x) y
