module Ch7.Grabbag where

addOneIfOdd :: Integral p => p -> p
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where
    f n = n + 1

addOneIfOdd' :: Integral p => p -> p
addOneIfOdd' n = case odd n of
  True -> f n
  False -> n
  where
    f = \n -> n + 1

addFive :: (Num a, Ord a) => a -> a -> a
addFive x y = (if x > y then y else x) + 5

addFive' :: Integer -> Integer -> Integer
addFive' = \x y -> (if x > y then y else x) + 5

mflip :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
mflip f = \x -> \y -> f y x

mflip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
mflip' f x y = f y x

plus0 :: [Char] -> [Char]
plus0 n = n ++ "0"

plus0' :: [Char] -> [Char]
plus0' = \n -> n ++ "0"
