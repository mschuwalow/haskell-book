module Ch9.Exercise where

import Data.Bool (bool)
import Data.Char

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eft :: (Ord a, Enum a) => a -> a -> [a]
eft start end =
  reverse (go start [])
  where
    go start acc
      | start > end = []
      | start == end = start : acc
      | otherwise = go (succ start) (start : acc)

eftChar :: Char -> Char -> [Char]
eftChar = eft

cut :: Eq a => [a] -> a -> [[a]]
cut x y =
  go x []
  where
    go [] acc = reverse acc
    go remainder acc =
      go next (firstword : acc)
      where
        firstword = takeWhile (/= y) $ remainder
        next = dropWhile (== y) . dropWhile (/= y) $ remainder

firstSen :: [Char]
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: [Char]
secondSen = "In the forests of the night\n"

thirdSen :: [Char]
thirdSen = "What immortal hand or eye\n"

fourthSen :: [Char]
fourthSen = "Could frame thy fearful symmetry?"

sentences :: [Char]
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines sentences = cut sentences '\n'

shouldEqual :: [[Char]]
shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
    "Are they equal? "
      ++ show
        ( myLines sentences
            == shouldEqual
        )

--List comprehensions

mySqr :: [Integer]
mySqr = [x ^ 2 | x <- [1 .. 10]]

ex :: [Integer]
ex = [x | x <- mySqr, rem x 2 == 0]

exTwo :: [(Integer, Integer)]
exTwo = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

exThree :: [(Integer, Integer)]
exThree = take 5 [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

--Square Cube
mySrqTwo :: [Integer]
mySrqTwo = [x ^ 2 | x <- [1 .. 5]]

myCube :: [Integer]
myCube = [y ^ 3 | y <- [1 .. 5]]

qOne :: [(Integer, Integer)]
qOne = [(x, y) | x <- mySrqTwo, y <- myCube]

qTwo :: [(Integer, Integer)]
qTwo = [(x, y) | x <- mySrqTwo, y <- myCube, x < 50, y < 50]

qThree :: Int
qThree = length [(x, y) | x <- mySrqTwo, y <- myCube, x < 50, y < 50]

-- More bottoms
itIsMystery :: [Char] -> [Bool]
itIsMystery = map (\x -> elem x "aeiou")

myMap :: (t -> b) -> (t -> b) -> (t -> Bool) -> [t] -> [b]
myMap f g h =
  map (\x -> bool (f x) (g x) (h x))

a :: [Integer]
a = myMap (\x -> - x) id (== 3) [1, 2, 3]

-- Exercises: Filtering

multOfThree :: [Integer]
multOfThree = [x | x <- [1 .. 30], rem x 3 == 0]

howLong :: Int
howLong = length multOfThree

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y : ys)
  | x == y = remove x ys
  | otherwise = y : remove x ys

myElem :: Eq t => t -> [t] -> Bool
myElem _ [] = False
myElem y (x : xs) = x == y || myElem y xs

myFilter :: [String]
myFilter = [x | x <- words [], notElem x wordsToFilter]
  where
    wordsToFilter =
      [ "a",
        "an"
      ]

-- Zipping exercises
zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith' (,)

zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (a : as) (b : bs) = f a b : zipWith' f as bs

--Chapter Exercises
onlyCap :: [Char] -> [Char]
onlyCap = filter isUpper

makeFirstCap :: [Char] -> [Char]
makeFirstCap [] = []
makeFirstCap (x : xs) = toUpper x : xs

makeAllCap :: [Char] -> [Char]
makeAllCap [] = []
makeAllCap (x : xs) = toUpper x : makeAllCap xs

makeFirstCap' :: [Char] -> Char
makeFirstCap' = toUpper . head

-- writing your own standard functions

--1. myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

-- 2. myAny returns True if a -> Bool applied to any of the values in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = myOr (map f xs)

--3. redo myelem
myElemTwo :: Eq a => a -> [a] -> Bool
myElemTwo y = any (== y)

--4.Implement myReverse.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

--5. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

--6. squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = squish (map f xs)

--7. squishAgain flattens a list of lists into a list. This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

--8. myMaximumBy takes a comparison function and a list and returns the greatest element
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f [] = error "non"
myMaximumBy f (x : []) = x
myMaximumBy f (x : xs) = case f x big of
  GT -> x
  LT -> big
  EQ -> x
  where
    big = myMaximumBy f xs

--9. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f [] = error "non"
myMinimumBy f (x : []) = x
myMinimumBy f (x : xs) = case f x small of
  GT -> small
  LT -> x
  EQ -> x
  where
    small = myMinimumBy f xs

--10. Using the myMinimumBy and myMaximumBy functions, write your own versions of maximum and minimum.
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
