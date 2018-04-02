{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module WriterT where

import           Control.Monad
import           Data.Monoid

newtype MyWriterT w m a = MyWriterT
  { runMyWriterT :: m (a, w)
  }

class MonadTrans t where
  lift :: Monad m => m a -> t m a

class (Monoid w, Monad m) =>
      (MonadWriter w m) where
  tell :: (Monoid w, Monad m) => w -> m ()

instance (Monoid w, Monad m) => Functor (MyWriterT w m) where
  fmap = liftM

instance (Monoid w, Monad m) => Applicative (MyWriterT w m) where
  pure = MyWriterT . pure . flip (,) mempty
  (<*>) = ap

instance (Monoid w, Monad m) => Monad (MyWriterT w m) where
  return = pure
  (MyWriterT m) >>= k =
    MyWriterT $ do
      (a, w) <- m
      (a', w') <- runMyWriterT (k a)
      return (a', w <> w')

instance (Monoid w) => MonadTrans (MyWriterT w) where
  lift = MyWriterT . fmap (flip (,) mempty)

instance (Monoid w, Monad m) => MonadWriter w (MyWriterT w m) where
  tell = MyWriterT . pure . (,) ()
