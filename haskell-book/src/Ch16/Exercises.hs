{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Ch16.Exercises where

import Ch15.Laws
import Test.QuickCheck

data Sum b a =
  First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- / Second exercise

data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

instance (Arbitrary a, Arbitrary b)
      => Arbitrary (Quant a b) where
        arbitrary = frequency [
            (1, return Finance),
            (2, fmap Desk arbitrary),
            (2, fmap Bloor arbitrary)
          ]

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

instance Arbitrary a
      => Arbitrary (K a b) where
        arbitrary = fmap K arbitrary

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K $ f a

instance (Arbitrary b)
      => Arbitrary (Flip K a b) where
        arbitrary = do
          a <- arbitrary
          return $ Flip $ K a

data EvilGoateeConst a b =
  GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst $ f a

instance Arbitrary b
      => Arbitrary (EvilGoateeConst a b) where
        arbitrary = fmap GoatyConst arbitrary

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f
      => Functor (LiftItOut f) where
        fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

instance Arbitrary (f a)
      => Arbitrary (LiftItOut f a) where
        arbitrary = fmap LiftItOut arbitrary

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
