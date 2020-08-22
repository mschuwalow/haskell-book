{-# LANGUAGE TypeApplications #-}

module Ch15.Optional where

import Ch15.Laws
import Test.QuickCheck

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance
  Semigroup a =>
  Semigroup (Optional a)
  where
  (<>) Nada a = a
  (<>) a Nada = a
  (<>) (Only a) (Only a') = Only $ a <> a'

instance
  Monoid a =>
  Monoid (Optional a)
  where
  mempty = Nada

instance
  Arbitrary a =>
  Arbitrary (Optional a)
  where
  arbitrary =
    frequency
      [ (1, return Nada),
        (2, fmap Only arbitrary)
      ]

newtype First' a
  = First' {unFirst' :: Optional a}
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only a)) _ = First' (Only a)
  (<>) (First' Nada) second = second

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = fmap First' arbitrary

runQc :: IO ()
runQc = do
  quickCheck (semigroupAssoc @(Optional String))
  quickCheck (monoidLeftIdentity @(Optional String))
  quickCheck (monoidRightIdentity @(Optional String))
  quickCheck (semigroupAssoc @(First' Int))
  quickCheck (monoidLeftIdentity @(First' Int))
  quickCheck (monoidRightIdentity @(First' Int))
