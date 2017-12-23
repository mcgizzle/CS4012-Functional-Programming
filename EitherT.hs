{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies ,FlexibleInstances #-}
module EitherT where

import Control.Monad

newtype MyEitherT l m r = MyEitherT { runMyEitherT :: m (Either l r)}

class MonadTrans t where
        lift :: Monad m => m a -> t m a

class (Show e, Monad m) => (MonadError e m) where
        eFail :: e -> m a
        eHandle :: m a -> (e -> m a) -> m a

instance (Monad m) => Functor (MyEitherT l m) where
        fmap = liftM

instance (Monad m) => Applicative (MyEitherT l m) where
        pure  = MyEitherT . pure . Right
        (<*>) = ap

instance (Monad m) => Monad (MyEitherT l m) where
        return = pure
        a >>= b = MyEitherT $ do
                a' <- runMyEitherT a
                case a' of
                  Left err  -> return $ Left err
                  Right res -> runMyEitherT $ b res

instance MonadTrans (MyEitherT l) where
        lift m = MyEitherT $ do
                m' <- m
                return $ Right m'

instance (Show l, Monad m) => MonadError l (MyEitherT l m) where
        eFail l = MyEitherT $ return $ Left l
        eHandle m1 m2 = MyEitherT $ do
                m1' <- runMyEitherT m1
                case m1' of
                  Right res -> return m1'
                  Left err  -> runMyEitherT $ m2 err
