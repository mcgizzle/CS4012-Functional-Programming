{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module MyReaderT where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader 

newtype MyReaderT r m a = MyReaderT { runMyReaderT :: r -> m a }

instance Monad m => Functor (MyReaderT r m) where
        fmap = liftM

instance Monad m => Applicative (MyReaderT r m) where
        pure a = MyReaderT $ \ _ -> return a

instance Monad m => Monad (MyReaderT r m) where
        return = pure
        a >>= b = MyReaderT $ \ r -> do
                a' <- runMyReaderT a r
                runMyReaderT (b a') r

instance MonadTrans (MyReaderT r) where
        lift m = MyReaderT $ \ _ -> m
                
instance Monad m => MonadReader r (MyReaderT r m) where
        ask = MyReaderT $ \ r -> return r 
