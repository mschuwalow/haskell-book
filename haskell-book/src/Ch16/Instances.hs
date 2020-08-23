{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
module Ch16.Instances where

import Ch15.Laws
import Test.QuickCheck


newtype Identity a =
  Identity a
  deriving (Eq, Show)
  deriving Arbitrary via a

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a
      => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (Two a b) where
        arbitrary = do
          a <- arbitrary
          b <- arbitrary
          return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c)
      => Arbitrary (Three a b c) where
        arbitrary = do
          a <- arbitrary
          b <- arbitrary
          c <- arbitrary
          return $ Three a b c

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (Three' a b) where
        arbitrary = do
          a <- arbitrary
          b <- arbitrary
          b' <- arbitrary
          return $ Three' a b b'

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
      => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (Four' a b) where
        arbitrary = do
          a <- arbitrary
          b <- arbitrary
          c <- arbitrary
          d <- arbitrary
          return $ Four' a b c d

data Possibly a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers $ f a

instance Arbitrary a
      => Arbitrary (Possibly a) where
        arbitrary = frequency [
            (1, return LolNope),
            (2, fmap Yeppers arbitrary)
          ]

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (Sum a b) where
        arbitrary = frequency [
            (1, fmap First arbitrary),
            (2, fmap Second arbitrary)
          ]

runQc :: IO ()
runQc = do
  quickCheck $ functorIdentity @Identity @Int
  quickCheck $ functorCompose @Identity @Int @Int @Int
  quickCheck $ functorIdentity @Pair @Int
  quickCheck $ functorCompose @Pair @Int @Int @Int
  quickCheck $ functorIdentity @(Two Int) @Int
  quickCheck $ functorCompose @(Two Int) @Int @Int @Int
  quickCheck $ functorIdentity @(Three Int Int) @Int
  quickCheck $ functorCompose @(Three Int Int) @Int @Int @Int
  quickCheck $ functorIdentity @(Three' Int) @Int
  quickCheck $ functorCompose @(Three' Int) @Int @Int @Int
  quickCheck $ functorIdentity @(Four Int Int Int) @Int
  quickCheck $ functorCompose @(Four Int Int Int) @Int @Int @Int
  quickCheck $ functorIdentity @(Four' Int) @Int
  quickCheck $ functorCompose @(Four' Int) @Int @Int @Int
  quickCheck $ functorIdentity @Possibly @Int
  quickCheck $ functorCompose @Possibly @Int @Int @Int
  quickCheck $ functorIdentity @(Sum Int) @Int
  quickCheck $ functorCompose @(Sum Int) @Int @Int @Int
