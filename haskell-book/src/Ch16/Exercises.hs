{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Ch16.Exercises where

import Ch15.Laws
import Control.Applicative
import Test.QuickCheck

data Sum b a
  = First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

data Company a c b
  = DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a
  = L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- / Second exercise

data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (Quant a b)
  where
  arbitrary =
    frequency
      [ (1, return Finance),
        (2, fmap Desk arbitrary),
        (2, fmap Bloor arbitrary)
      ]

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

instance
  Arbitrary a =>
  Arbitrary (K a b)
  where
  arbitrary = fmap K arbitrary

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K $ f a

instance
  (Arbitrary b) =>
  Arbitrary (Flip K a b)
  where
  arbitrary = do
    a <- arbitrary
    return $ Flip $ K a

data EvilGoateeConst a b
  = GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst $ f a

instance
  Arbitrary b =>
  Arbitrary (EvilGoateeConst a b)
  where
  arbitrary = fmap GoatyConst arbitrary

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance
  Functor f =>
  Functor (LiftItOut f)
  where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

instance
  Arbitrary (f a) =>
  Arbitrary (LiftItOut f a)
  where
  arbitrary = fmap LiftItOut arbitrary

data Parappa f g a
  = DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance
  (Functor f, Functor g) =>
  Functor (Parappa f g)
  where
  fmap f (DaWrappa (fa) (ga)) = DaWrappa (fmap f (fa)) (fmap f (ga))

instance
  (Arbitrary (f a), Arbitrary (g a)) =>
  Arbitrary (Parappa f g a)
  where
  arbitrary = do
    fa <- arbitrary
    ga <- arbitrary
    return $ DaWrappa fa ga

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance
  (Functor g) =>
  (Functor (IgnoreOne f g a))
  where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

instance
  (Arbitrary (f a), Arbitrary (g b)) =>
  Arbitrary (IgnoreOne f g a b)
  where
  arbitrary = do
    fa <- arbitrary
    gb <- arbitrary
    return $ IgnoringSomething fa gb

data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance
  (Functor g) =>
  (Functor (Notorious g o a))
  where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

instance
  (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) =>
  Arbitrary (Notorious g o a t)
  where
  arbitrary = liftA3 Notorious arbitrary arbitrary arbitrary

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance
  Arbitrary a =>
  (Arbitrary (List a))
  where
  arbitrary = sized go
    where
      go 0 = pure Nil
      go n = do
        xs <- go (n - 1)
        x <- arbitrary
        return (Cons x xs)

data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats
      (GoatLord a)
      (GoatLord a)
      (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

instance
  Arbitrary a =>
  Arbitrary (GoatLord a)
  where
  arbitrary = sized go
    where
      go 0 = pure NoGoat
      go n =
        frequency
          [ (1, pure NoGoat),
            (2, fmap OneGoat arbitrary),
            (1, liftA3 MoreGoats (go (n -1)) (go (n -1)) (go (n - 1)))
          ]

data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g) = Read $ f . g

runQc :: IO ()
runQc = do
  quickCheck $ functorIdentity @(Quant Int) @Int
  quickCheck $ functorCompose @(Quant Int) @Int @Int @Int
  quickCheck $ functorIdentity @(K Int) @Int
  quickCheck $ functorCompose @(K Int) @Int @Int @Int
  quickCheck $ functorIdentity @(Flip K Int) @Int
  quickCheck $ functorCompose @(Flip K Int) @Int @Int @Int
  quickCheck $ functorIdentity @(EvilGoateeConst Int) @Int
  quickCheck $ functorCompose @(EvilGoateeConst Int) @Int @Int @Int
  quickCheck $ functorIdentity @(LiftItOut []) @Int
  quickCheck $ functorCompose @(LiftItOut []) @Int @Int @Int
  quickCheck $ functorIdentity @(Parappa [] []) @Int
  quickCheck $ functorCompose @(Parappa [] []) @Int @Int @Int
  quickCheck $ functorIdentity @(IgnoreOne [] [] Int) @Int
  quickCheck $ functorCompose @(IgnoreOne [] [] Int) @Int @Int @Int
  quickCheck $ functorIdentity @(Notorious [] Int Int) @Int
  quickCheck $ functorCompose @(Notorious [] Int Int) @Int @Int @Int
  quickCheck $ functorIdentity @List @Int
  quickCheck $ functorCompose @List @Int @Int @Int
  quickCheck $ functorIdentity @GoatLord @Int
  quickCheck $ functorCompose @GoatLord @Int @Int @Int
