module Ch21.Tree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node t1 a t2) = Node (fmap f t1) (f a) (fmap f t2)

instance Foldable Tree where
  foldr _ b Empty = b
  foldr f b (Leaf a) = f a b
  foldr f b (Node t1 a t2) = foldr f (f a $ foldr f b t2) t1

instance Traversable Tree where
  sequenceA Empty = pure Empty
  sequenceA (Leaf a) = fmap Leaf a
  sequenceA (Node t1 a t2) = Node <$> (sequenceA t1) <*> a <*> (sequenceA t2)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized go
    where
      go n
        | n <= 0 = pure Empty
        | otherwise = frequency [(1, pure Empty), (1, fmap Leaf arbitrary), (2, Node <$> (go $ n `div` 2) <*> arbitrary <*> (go $ n `div` 2))]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

runQc :: IO ()
runQc =
  quickBatch $ traversable (undefined :: Tree (Int, Int, [Int]))
