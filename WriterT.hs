module WriterT where

import Control.Monad.Trans
import Control.Monad
import Control.Monad.Writer
import Data.Monoid

newtype MyWriterT w m a = MyWriterT {  runMyWriterT :: m (a,w)  }

instance (Monoid w, Monad m) => Functor (MyWriterT w m) where
        fmap = liftM

instance (Monoid w, Monad m) => Applicative (MyWriterT w m) where
        pure a = MyWriterT $ pure (a,mempty)
        (<*>)  = ap

instance (Monoid w, Monad m) => Monad (MyWriterT w m) where
        return = pure
        (>>=) m k = MyWriterT $ do
                (a, w) <- runMyWriterT m
                (a',w') <- runMyWriterT (k a)
                return (a',w `mappend` w')

instance Monoid w => MonadTrans (MyWriterT w) where
        lift m = MyWriterT $ do
                a <- m
                return (a,mempty)


--tell :: w -> MyWriterT ((),w)
tell w = MyWriterT $ return ((),w)
