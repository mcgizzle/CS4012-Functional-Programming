module ErrorT where

import Control.Monad
import Control.Monad.Trans
import Data.Either

newtype MyErrorT m a = MyErrorT { runMyErrorT :: m (Either String a) }

instance Monad m => Monad (MyErrorT m) where
        return = pure
        x >>= y = MyErrorT $ do
                res <- x
                case res of
                  Right val -> return $ runMyErrorT $ y res 
                  Left err  -> return $ Left err

instance Monad m => Functor (MyErrorT m) where
        fmap = liftM 

instance Monad m => Applicative (MyErrorT m) where
        pure a = MyErrorT $ return $ Right a
        (<*>) = ap

instance MonadTrans MyErrorT where
        lift m = MyErrorT $ liftM Right m


