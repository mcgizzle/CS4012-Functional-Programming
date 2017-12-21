{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
import Control.Monad
import Control.Monad.Trans

newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a,s) }

instance Monad m => Functor (MyStateT s m) where
        fmap = liftM

instance Monad m => Applicative (MyStateT s m) where
        pure a = MyStateT $ \ s -> return (a,s)
        (<*>) = ap

instance Monad m => Monad (MyStateT s m) where
        return = pure
        x >>= y = MyStateT $ \ s -> do
                (x',s') <- runMyStateT x s 
                runMyStateT (y x') s'


instance MonadTrans (MyStateT s) where
        lift x = MyStateT $ \ s -> do
                x' <- x
                return (x',s)

get = MyStateT $ \ s -> return (s,s)
put s = MyStateT $ \ _ -> return ((),s)
          
