module Ch25.Compose where

newtype Compose f g a = Compose {getCompose :: f (g a)}
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) =
    Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  (Compose fgh) <*> (Compose fga) =
    Compose $ ((<*>) <$> fgh) <*> fga
  pure a =
    Compose $ (pure (pure a))

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldr h b (Compose fga) =
    foldr (\ga b -> foldr h b ga) b fga

instance
  (Traversable f, Traversable g) =>
  Traversable (Compose f g)
  where
  traverse h (Compose fga) =
    fmap Compose . traverse (traverse h) $ fga
