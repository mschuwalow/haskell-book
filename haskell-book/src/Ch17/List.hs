module Ch17.List where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

instance
  Eq a =>
  EqProp (List a)
  where
  (=-=) = eq

instance
  Arbitrary a =>
  Arbitrary (List a)
  where
  arbitrary = sized go
    where
      go 0 = pure Nil
      go n = Cons <$> arbitrary <*> (go (n - 1))

runQc :: IO ()
runQc = do
  quickBatch $ applicative (Cons ("", "", "") Nil)
