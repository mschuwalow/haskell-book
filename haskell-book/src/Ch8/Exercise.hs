module Ch8.Exercise where

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 1 f x = f x
applyTimes n f x = f (applyTimes (n - 1) f x)

-- applyTimes 5 (+1) 5

--chapter exercise
cattyConny :: String -> String -> String
cattyConny x y = x ++ "mrow" ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

--Recursion
--2.
rr :: (Eq a, Num a) => a -> a
rr 0 = 0
rr x = x + rr (x -1)

--3.
tt :: (Integral a) => a -> a -> a
tt x 1 = x
tt x y = x + (tt x (y - 1))

--extract
suml :: [Integer] -> Integer
suml [] = 0
suml (x : xs) = x + suml xs

data Tree a
  = Leaf a
  | Node (Tree a) a (Tree a)

sumT :: (a -> a -> a) -> Tree a -> a
sumT _ (Leaf a) = a
sumT f (Node t1 a t2) = f a (f (sumT f t1) (sumT f t2))

digits :: (Integral t, Num p) => t -> p
digits x
  | x < 10 && x > -10 = 1
  | otherwise = 1 + digits (x `div` 10)

digits' :: (Num p, Integral t) => t -> p
digits' a
  | a < 10 = 1
  | otherwise = digits' (a `div` 10) + 1

--fixing dividedBy
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

data DividedResult = Result Integer | DividedByZero

dividedResult :: Integer -> Integer -> DividedResult
dividedResult x y
  | y == 0 = DividedByZero
  | otherwise = Result (x `div` y)

--McCarthy 91 function
mc91 :: (Eq a, Integral a) => a -> a
mc91 x
  | x > 100 = (x - 10)
  | otherwise = mc91 . mc91 $ (x + 11)
--Numbers into words
