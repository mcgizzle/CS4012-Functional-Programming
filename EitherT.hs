module EitherT where

import Control.Monad
import Control.Monad.Trans

newtype MyEitherT l m r = MyEitherT { runMyEitherT :: m (Either l r)}

instance (Monad m) => Functor (MyEitherT l m) where
        fmap = liftM

instance (Monad m) => Applicative (MyEitherT l m) where
        pure = MyEitherT . pure . Right

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
