{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module ReaderT where

import           Control.Monad

newtype MyReaderT r m a = MyReaderT
  { runMyReaderT :: r -> m a
  }

class MonadTrans t where
  lift :: m a -> t m a

class Monad m =>
      (MonadReader r m) where
  ask :: m r
  asks :: (r -> a) -> m a

instance Monad m => Functor (MyReaderT r m) where
  fmap = liftM

instance Monad m => Applicative (MyReaderT r m) where
  pure a = MyReaderT $ \_ -> return a
  (<*>) = ap

instance Monad m => Monad (MyReaderT r m) where
  return = pure
  (MyReaderT a) >>= b = MyReaderT $ \r -> a r >>= flip runMyReaderT r . b

instance MonadTrans (MyReaderT r) where
  lift m = MyReaderT $ \_ -> m

instance Monad m => MonadReader r (MyReaderT r m) where
  ask = MyReaderT $ \r -> return r
  asks a = MyReaderT $ \r -> return $ a r
