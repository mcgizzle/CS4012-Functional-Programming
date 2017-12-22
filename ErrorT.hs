module ErrorT where

import Control.Monad
import Control.Monad.Trans
import Data.Either

newtype MyErrorT m a = MyErrorT { runMyErrorT :: m (Either String a) }

instance Monad m => Monad (MyErrorT m) where
        return = pure
        x >>= y = MyErrorT $ do
                res <- runMyErrorT x
                case res of
                  Right val -> runMyErrorT $ y val 
                  Left err  -> return $ Left err

instance Monad m => Functor (MyErrorT m) where
        fmap = liftM 

instance Monad m => Applicative (MyErrorT m) where
        pure = MyErrorT . return . Right
        (<*>) = ap

instance MonadTrans MyErrorT where
        lift m = MyErrorT $ do
                m' <- m
                return $ Right m'

throw :: Monad m => String -> MyErrorT m a
throw = MyErrorT . return . Left

catch :: Monad m => MyErrorT m a -> (String -> MyErrorT m a) -> MyErrorT m a 
catch m c = MyErrorT $ do
                m' <- runMyErrorT m
                case m' of
                  Left err -> runMyErrorT $ c err
                  Right _  -> runMyErrorT m
