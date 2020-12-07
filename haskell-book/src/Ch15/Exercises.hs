{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Ch15.Exercises where

import Ch15.Laws
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

newtype Identity a
  = Identity a
  deriving (Eq, Show)
  deriving (Arbitrary, Semigroup, Monoid) via a

data Two a b = Two a b deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b) =>
  Semigroup (Two a b)
  where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance
  (Monoid a, Monoid b) =>
  Monoid (Two a b)
  where
  mempty = Two mempty mempty

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b)
  where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

data Three a b c = Three a b c deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c)
  where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance
  (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c)
  where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

data Four a b c d = Four a b c d deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d)
  where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance
  (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d)
  where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)
  deriving (Arbitrary) via Bool

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj $ a && b

instance Monoid BoolConj where
  mempty = BoolConj True

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)
  deriving (Arbitrary) via Bool

instance Semigroup BoolDisj where
  (BoolDisj a) <> (BoolDisj b) = BoolDisj $ a || b

instance Monoid BoolDisj where
  mempty = BoolDisj False

data Or a b
  = Fst a
  | Snd a
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  r@(Snd _) <> _ = r
  _ <> r@(Snd _) = r
  _ <> b = b

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (Or a b)
  where
  arbitrary =
    frequency
      [ (1, fmap Fst arbitrary),
        (2, fmap Snd arbitrary)
      ]

newtype Combine a b = Combine {unCombine :: (a -> b)}
  deriving (Arbitrary) via (a -> b)

instance
  Semigroup b =>
  Semigroup (Combine a b)
  where
  (Combine f) <> (Combine g) = Combine $ f <> g

instance
  Monoid b =>
  Monoid (Combine a b)
  where
  mempty = Combine $ \_ -> mempty

newtype Comp a = Comp {unComp :: (a -> a)}
  deriving (Arbitrary) via (a -> a)

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp $ f . g

instance Monoid (Comp a) where
  mempty = Comp id

data Validation a b
  = Failure' a
  | Success' b
  deriving (Eq, Show)

instance
  Semigroup a =>
  Semigroup (Validation a b)
  where
  (Failure' a) <> (Failure' b) = Failure' $ a <> b
  r@(Success' _) <> _ = r
  _ <> r@(Success' _) = r

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (Validation a b)
  where
  arbitrary =
    frequency
      [ (1, fmap Failure' arbitrary),
        (2, fmap Success' arbitrary)
      ]

-- / check properties
runQc :: IO ()
runQc = do
  quickCheck $ semigroupAssoc @Trivial
  quickCheck $ monoidLeftIdentity @Trivial
  quickCheck $ monoidRightIdentity @Trivial
  quickCheck $ semigroupAssoc @(Identity String)
  quickCheck $ monoidLeftIdentity @(Identity String)
  quickCheck $ monoidRightIdentity @(Identity String)
  quickCheck $ semigroupAssoc @(Two String String)
  quickCheck $ monoidLeftIdentity @(Two String String)
  quickCheck $ monoidRightIdentity @(Two String String)
  quickCheck $ semigroupAssoc @(Three String String String)
  quickCheck $ semigroupAssoc @(Four String String String String)
  quickCheck $ semigroupAssoc @BoolConj
  quickCheck $ monoidLeftIdentity @BoolConj
  quickCheck $ monoidRightIdentity @BoolConj
  quickCheck $ semigroupAssoc @BoolDisj
  quickCheck $ monoidLeftIdentity @BoolDisj
  quickCheck $ monoidRightIdentity @BoolDisj
  quickCheck $ semigroupAssoc @(Or Int Int)
  -- We need effectful equality
  -- quickCheck $ semigroupAssoc @(Combine Int String)
  quickCheck $ semigroupAssoc @(Validation String Int)
