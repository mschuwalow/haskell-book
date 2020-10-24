module Ch7.CasePractice where

--1.functionC x y = if (x > y) then x else y
functionC :: Ord p => p -> p -> p
functionC x y =
  case x > y of
    True -> x
    False -> y

--2.ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2 :: Integral p => p -> p
ifEvenAdd2 n =
  case mod n 2 == 0 of
    True -> (n + 2)
    False -> n

nums :: (Ord a, Num a, Num p) => a -> p
nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
