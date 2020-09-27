{-# LANGUAGE FlexibleContexts #-}

module Ch21.SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

instance
  ( Functor n,
    Arbitrary (n a),
    Arbitrary a
  ) =>
  Arbitrary (S n a)
  where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldr f b (S na a) = foldr f (f a b) na

instance Traversable n => Traversable (S n) where
  sequenceA (S na a) = S <$> (sequenceA na) <*> a

examples :: IO [S [] Int]
examples = sample' arbitrary

runQc :: IO ()
runQc = do
  quickBatch $ traversable (undefined :: S [] (Int, Int, [Int]))
