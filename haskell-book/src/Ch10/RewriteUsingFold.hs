module Ch10.RewriteUsingFold where

import Data.Bool

myOr :: [Bool] -> Bool
myOr =
  foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f =
  myOr . map f

myElem :: Eq a => a -> [a] -> Bool
myElem x =
  myAny (== x)

myElem' x =
  foldr (\next acc -> acc || (next == x)) False

myReverse :: [a] -> [a]
myReverse =
  foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f =
  foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr g []
  where
    g next acc = bool acc (next : acc) (f next)

squish :: [[a]] -> [a]
squish =
  foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f =
  squish . myMap f

squishAgain :: [[a]] -> [a]
squishAgain =
  squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp (x : xs) =
  foldl (\a b -> bool b a (LT == cmp a b)) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp (x : xs) =
  foldl (\a b -> bool b a (LT == cmp a b)) x xs
