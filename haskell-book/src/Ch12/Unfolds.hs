module Ch12.Unfolds where

myIterate :: (a -> a) -> a -> [a]
myIterate f initial =
  go initial
  where
    go a = a : (go $ f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f initial =
  go initial
  where
    go b =
      maybe [] (\(a, b') -> a : (go b')) $ f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f =
  myUnfoldr (\x -> Just (x, f x))
