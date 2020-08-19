module Ch12.MaybeLib where

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b _ Nothing = b
mayybee _ f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a =
  mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe (x : _) = Just x
listToMaybe _ = Nothing

maybeToList :: Maybe a -> [a]
maybeToList =
  mayybee [] (: [])

catMaybes :: [Maybe a] -> [a]
catMaybes =
  foldr f []
  where
    f (Just a) xs = a : xs
    f _ xs = xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe ((Just x) : xs) =
  fmap (x :)
    . flipMaybe
    $ xs
flipMaybe (Nothing : _) = Nothing
