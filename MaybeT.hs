import Control.Monad
import Control.Monad.Trans

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
        fmap = liftM

instance Monad m => Applicative (MaybeT m) where
        pure = MaybeT . return . Just

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

