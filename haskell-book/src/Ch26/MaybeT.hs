module Ch26.MaybeT where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance
  (Functor m) =>
  Functor (MaybeT m)
  where
  fmap f (MaybeT ma) =
    MaybeT $ (fmap . fmap) f ma

instance
  (Applicative m) =>
  Applicative (MaybeT m)
  where
  pure x = MaybeT (pure (pure x))
  (MaybeT fmab) <*> (MaybeT fa) =
    MaybeT $ fmamb <*> fa
    where
      fmamb = (<*>) <$> fmab

instance
  (Monad m) =>
  Monad (MaybeT m)
  where
  return = pure
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)

instance MonadTrans MaybeT where
  lift = MaybeT . fmap Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO
