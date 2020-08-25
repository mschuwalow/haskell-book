{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
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

applicativeIdentity :: (Eq (f b), Applicative f) => f b -> Bool
applicativeIdentity v =
  (pure id <*> v) == v

--applicativeHomomorphism :: (Eq (f a1), Applicative f) => (a2 -> a1) -> a2 -> Bool
--applicativeHomomorphism f x =
--  (pure f <*> (pure x) :: f a2) == pure (f x)

applicativeInterchange :: (Eq (f b), Applicative f) => f (Fun a b) -> a -> Bool
applicativeInterchange _u y =
  (u <*> pure y) == (pure ($ y) <*> u)
  where
    u = fmap applyFun _u

applicativeComposition :: (Eq (f b1), Applicative f) => f (Fun b2 b1) -> f (Fun a b2) -> f a -> Bool
applicativeComposition _u _v w =
  (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))
  where
    u = fmap applyFun _u
    v = fmap applyFun _v

