module MaybeT where

import Control.Monad

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

class MonadTrans t where
        lift :: Monad m => m a -> t m a

class Monad m => MonadError m where
        eFail :: m a
        eHandle :: m a -> m a -> m a

instance Monad m => Functor (MaybeT m) where
        fmap = liftM

instance Monad m => Applicative (MaybeT m) where
        pure = MaybeT . return . Just
        (<*>) = ap

instance Monad m => Monad (MaybeT m) where
        return = pure
        a >>= b = MaybeT $ do
                a' <- runMaybeT a
                case a' of
                  Just val -> runMaybeT $ b val
                  Nothing -> return Nothing

instance MonadTrans MaybeT where
        lift m = MaybeT $ do
                m' <- m
                return $ Just m'

instance Monad m => MonadError (MaybeT m) where
        eFail = MaybeT $ return Nothing
        eHandle m1 m2 = MaybeT $ do
                m1' <- runMaybeT m1
                case m1' of
                  Just _  -> return m1'
                  Nothing -> runMaybeT m2

