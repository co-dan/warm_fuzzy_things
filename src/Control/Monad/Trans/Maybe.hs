{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Maybe monad transformer
module Control.Monad.Trans.Maybe
       (MaybeT(..), module Control.Monad.Trans)
       where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
  -- (>>=) :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT $
            runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)
  -- return :: (Monad m) => a -> MaybeT m a
  return x = MaybeT $ return (Just x)

instance MonadTrans MaybeT where
  -- lift :: (Monad m) => m a -> MaybeT m a
  lift = MaybeT . liftM Just

instance (MonadReader r m) => MonadReader r (MaybeT m) where
  ask = lift ask
  local f x = MaybeT $ local f (runMaybeT x)

instance (MonadWriter w m) => MonadWriter w (MaybeT m) where
  tell = lift . tell
  writer = lift . writer
  listen x = MaybeT $ do
    (result,log) <- listen (runMaybeT x)
    case result of
      Nothing -> return Nothing
      Just v  -> return $ Just (v,log)
  pass x = MaybeT $ do
    a <- runMaybeT x
    case a of
      Nothing    -> return Nothing
      Just (v,f) -> pass $ return (Just v,f)

instance (MonadState s m) => MonadState s (MaybeT m) where
  put = lift . put
  get = lift get
