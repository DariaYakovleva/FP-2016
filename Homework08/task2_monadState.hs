--TASK
--Implement instance MonadState for StateT, ReaderT, MaybeT

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad.Trans.Class  (MonadTrans (..))
import Control.Monad.Trans.Maybe  (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State  (StateT (..))

main :: IO()
main = do
	putStrLn $ "HW8.2"

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

-- newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
instance (Monad m) => MonadState s (StateT s m) where
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_-> return ((), s)

-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
instance (MonadState s m) => MonadState s (ReaderT r m) where
    get   = lift get
    put = lift . put

--newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
instance (MonadState s m) => MonadState s (MaybeT m) where
    get   = lift get
    put = lift. put