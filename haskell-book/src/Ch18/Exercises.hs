{-# LANGUAGE DerivingVia #-}

module Ch18.Exercises where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind f =
  join . fmap f

data Nope a
  = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  _ <*> _ = NopeDotJpg
  pure _ = NopeDotJpg

instance Monad Nope where
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
  (First a) <*> _ = First a
  _ <*> (First a) = First a
  (Second f) <*> (Second b) = Second $ f b
  pure a = Second a

instance Monad (Sum a) where
  (First a) >>= _ = First a
  (Second b) >>= f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary =
    frequency
      [ (1, fmap First arbitrary),
        (1, fmap Second arbitrary)
      ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
  deriving (Arbitrary) via a

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure a = Identity a
  (Identity f) <*> (Identity a) = Identity $ f a

instance Monad Identity where
  (Identity a) >>= f = f a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> a = a
  (Cons a as) <> b = Cons a (as <> b)

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> as = (fmap f as) <> (fs <*> as)

instance Monad List where
  Nil >>= _ = Nil
  (Cons x xs) >>= f = (f x) <> (xs >>= f)

instance
  Arbitrary a =>
  Arbitrary (List a)
  where
  arbitrary = sized go
    where
      go n
        | n <= 0 = pure Nil
        | otherwise = Cons <$> arbitrary <*> (go (n `div` 2))

instance
  Eq a =>
  EqProp (List a)
  where
  (=-=) = eq

runQc :: IO ()
runQc = do
  quickBatch $ monad ((Second ("", "", "")) :: Sum String (String, String, String))
  quickBatch $ monad (NopeDotJpg :: Nope (String, String, String))
  quickBatch $ monad (Identity ("", "", ""))
  quickBatch $ monad (Cons ("", "", "") Nil)

j :: Monad m => m (m a) -> m a
j = (>>= id)

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

a' :: Monad m => m a -> m (a -> b) -> m b
a' = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = foldr go (return []) xs
  where
    go x acc = do
      x' <- f x
      acc' <- acc
      return $ x' : acc'

fliptype :: (Monad m) => [m a] -> m [a]
fliptype xs = meh xs id
