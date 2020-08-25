module Ch17.Validation where

import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance
  Semigroup e =>
  Semigroup (Validation e a)
  where
  (Success a) <> _ = Success a
  _ <> (Success a) = Success a
  (Failure e) <> (Failure e') = Failure $ e <> e'

instance
  Monoid e =>
  Monoid (Validation e a)
  where
  mempty = Failure mempty

instance
  Monoid e =>
  Applicative (Validation e)
  where
  pure = Success
  (Failure e) <*> (Failure e') = Failure $ e <> e'
  (Failure e) <*> _ = Failure e
  _ <*> (Failure e) = Failure e
  (Success f) <*> (Success x) = Success $ f x

instance
  (Arbitrary e, Arbitrary a) =>
  Arbitrary (Validation e a)
  where
  arbitrary =
    frequency
      [ (1, fmap Failure arbitrary),
        (1, fmap Success arbitrary)
      ]

instance
  (Eq e, Eq a) =>
  EqProp (Validation e a)
  where
  (=-=) = eq

runQc :: IO ()
runQc =
  quickBatch $ applicative (Success ("", "", "") :: (Validation String (String, String, String)))
