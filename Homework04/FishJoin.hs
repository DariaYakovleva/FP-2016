{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

import Prelude hiding (Monad, (>>=), return)

main:: IO()
main = do
	putStrLn "HW4.3"
-- split into 5 files
class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

class Monad m where   -- m :: * -> *
    return :: a -> m a                  -- return
    (>>=)  :: m a -> (a -> m b) -> m b  -- bind

instance Monad m => MonadFish m where
	returnFish = return
	f >=> g = \m -> ((return m >>= f) >>= g)

--instance Monad     m => MonadJoin m where ...
--instance MonadFish m => Monad     m where ...
--instance MonadFish m => MonadJoin m where ...