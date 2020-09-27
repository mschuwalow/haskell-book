{-# LANGUAGE DerivingVia #-}

module Ch21.Exercise where

import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
  deriving (EqProp, Arbitrary) via a

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldr f b (Identity a) =
    f a b

instance Traversable Identity where
  sequenceA (Identity fa) = fmap Identity fa

newtype Constant a b
  = Constant {getConstant :: a}
  deriving (Eq, Show, Ord)
  deriving (EqProp, Arbitrary) via a

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ b _ = b

instance Traversable (Constant a) where
  sequenceA (Constant a) = pure $ Constant a

data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show, Ord)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldr _ b Nada = b
  foldr f b (Yep a) = f a b

instance Traversable Optional where
  sequenceA Nada = pure Nada
  sequenceA (Yep fa) = fmap Yep fa

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency
      [ (1, pure Nada),
        (1, fmap Yep arbitrary)
      ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show, Ord)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldr _ b Nil = b
  foldr f b (Cons a as) = f a (foldr f b as)

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = (Cons) <$> x <*> (sequenceA xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized go
    where
      go n
        | n <= 0 = pure Nil
        | otherwise = Cons <$> arbitrary <*> (go (n `div` 2))

instance Eq a => EqProp (List a) where
  (=-=) = eq

data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f b (Three _ _ a) = f a b

instance Traversable (Three a b) where
  sequenceA (Three a b c) = fmap (Three a b) c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary =
    liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

data Pair a b
  = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f b (Pair _ a) = f a b

instance Traversable (Pair a) where
  sequenceA (Pair a b) = fmap (Pair a) b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

data Big a b
  = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldr f b (Big _ a a') = f a . f a' $ b

instance Traversable (Big a) where
  sequenceA (Big a b b') = (Big a) <$> b <*> b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

data Bigger a b
  = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldr f b (Bigger _ a a' a'') = f a . f a' . f a'' $ b

instance Traversable (Bigger a) where
  sequenceA (Bigger a b b' b'') = (Bigger a) <$> b <*> b' <*> b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

runQc :: IO ()
runQc = do
  quickBatch $ traversable (undefined :: Identity (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Constant Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Optional (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: List (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Three Int Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Pair Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Big Int (Int, Int, [Int]))
  quickBatch $ traversable (undefined :: Bigger Int (Int, Int, [Int]))
