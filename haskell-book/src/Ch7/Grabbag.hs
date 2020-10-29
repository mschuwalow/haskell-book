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

-- excerise atrful dodgy
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Integer -> Integer
oneIsOne = dodgy 1

oneIsTwo :: Integer -> Integer
oneIsTwo = (flip dodgy) 2

-- excerise Grard Duty
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where
    y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
  | xs == reverse xs = True
  | otherwise = False

number :: (Ord a, Num a, Num p) => a -> p
number x
  | x < 0 = -1
  | x == 0 = 0
  | otherwise = 1

-- chapter excerise
tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = fst . (`divMod` 10) $ x
    d = snd . (`divMod` 10) $ xLast

hunsD :: Integer -> Integer
hunsD = (`mod` 10) . (`div` 100)

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> y
    False -> x

foldBool' :: p -> p -> Bool -> p
foldBool' x y b
  | b = y
  | otherwise = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show b, Read b) => b -> b
roundTrip' = read . show

main :: IO ()
main = do
  print (roundTrip 4)
  print (id 4)

roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' a = read (show a)
