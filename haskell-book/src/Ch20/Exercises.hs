module Ch20.Exercises where

import Data.Monoid

newtype Min a = Min {getMin :: Maybe a}

instance Ord a => Semigroup (Min a) where
  m <> Min Nothing = m
  Min Nothing <> m = m
  (Min (Just m1)) <> (Min (Just m2)) = Min . Just $ min m1 m2

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing

newtype Max a = Max {getMax :: Maybe a}

instance Ord a => Semigroup (Max a) where
  m <> Max Nothing = m
  Max Nothing <> m = m
  (Max (Just m1)) <> (Max (Just m2)) = Max . Just $ min m1 m2

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = getAny . foldMap (Any . (== a))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = getMin . foldMap (Min . Just)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = getMax . foldMap (Max . Just)

null' :: Foldable t => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: Foldable t => t a -> Int
length' = getSum . foldMap (\_ -> Sum 1)

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr _ b (Constant _) = b

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f b (Two _ a) = f a b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f b (Three _ _ a) = f a b

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f b (Three' _ a1 a2) = f a2 . f a1 $ b

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f b (Four' _ a1 a2 a3) = f a3 . f a2 . f a1 $ b

filterF ::
  ( Applicative f,
    Foldable t,
    Monoid (f a)
  ) =>
  (a -> Bool) ->
  t a ->
  f a
filterF f = foldr (\a acc -> if (f a) then (pure a <> acc) else acc) mempty
