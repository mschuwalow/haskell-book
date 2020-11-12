module Ch26.EitherT where

import Control.Monad.Trans.Class

newtype EitherT e m a
  = EitherT {runEitherT :: m (Either e a)}

instance
  Functor m =>
  Functor (EitherT e m)
  where
  fmap f (EitherT fa) =
    EitherT $ (fmap . fmap) f fa

instance
  Applicative m =>
  Applicative (EitherT e m)
  where
  pure a = EitherT $ pure (pure a)
  (EitherT feab) <*> (EitherT fea) =
    EitherT $ feaeb <*> fea
    where
      feaeb = (<*>) <$> feab

instance
  Monad m =>
  Monad (EitherT e m)
  where
  return = pure
  (EitherT fea) >>= f =
    EitherT $ do
      v <- fea
      case v of
        Left e -> return $ Left e
        Right a -> runEitherT (f a)

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT ::
  (Functor m) =>
  EitherT e m a ->
  EitherT a m e
swapEitherT (EitherT fea) =
  EitherT $ fmap swapEither fea

eitherT ::
  Monad m =>
  (a -> m c) ->
  (b -> m c) ->
  EitherT a m b ->
  m c
eitherT f g (EitherT mea) = do
  v <- mea
  case v of
    Left a -> f a
    Right b -> g b
