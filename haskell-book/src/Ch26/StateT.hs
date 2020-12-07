{-# LANGUAGE TupleSections #-}

module Ch26.StateT where

import Control.Monad (ap)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor (first)

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance
  Functor m =>
  Functor (StateT s m)
  where
  fmap f (StateT fma) =
    StateT $ \s -> fmap (first f) (fma s)

instance
  Monad m =>
  Applicative (StateT s m)
  where
  pure a = StateT $ \s -> pure (a, s)
  (<*>) = ap

instance
  (Monad m) =>
  Monad (StateT s m)
  where
  return = pure
  (StateT fma) >>= f =
    StateT $ \s -> do
      (a, s) <- fma s
      (b, s) <- runStateT (f a) s
      return (b, s)

instance MonadTrans (StateT s) where
  lift fa = StateT $ \s -> fmap (,s) fa

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
