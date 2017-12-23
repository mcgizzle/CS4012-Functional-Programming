{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module ErrorT where

import Control.Monad
import Data.Either

newtype MyErrorT l m r = MyErrorT { runMyErrorT :: m (Either l r) }

class MonadTrans t where
        lift :: Monad m => m a -> t m a

class Monad m => MonadException e m where
        throw :: e -> m a
        catch :: m a -> (e -> m a) -> m a

instance Monad m => Monad (MyErrorT l m) where
        return = pure
        x >>= y = MyErrorT $ do
                res <- runMyErrorT x
                case res of
                  Right val -> runMyErrorT $ y val 
                  Left err  -> return $ Left err

instance Monad m => Functor (MyErrorT l m) where
        fmap = liftM 

instance Monad m => Applicative (MyErrorT l m) where
        pure = MyErrorT . return . Right
        (<*>) = ap

instance MonadTrans (MyErrorT l) where
        lift m = MyErrorT $ do
                m' <- m
                return $ Right m'

instance Monad m => MonadException l (MyErrorT l m) where 
        throw = MyErrorT . return . Left
        catch m1 m2 = MyErrorT $ do
                m1' <- runMyErrorT m1
                case m1' of
                  Left err -> runMyErrorT $ m2 err
                  Right _  -> return m1'
