{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Ch15.Laws where

import Test.QuickCheck

semigroupAssoc :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq a, Monoid a) => a -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq a, Monoid a) => a -> Bool
monoidRightIdentity a = (a <> mempty) == a

functorIdentity :: (Eq (f b), Functor f) => f b -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f b), Functor f) => Fun a1 a2 -> Fun a2 b -> f a1 -> Bool
functorCompose f' g' x =
  (fmap g (fmap f x)) == (fmap (g . f) x)
  where
    f = applyFun f'
    g = applyFun g'
