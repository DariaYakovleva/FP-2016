{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

import Prelude hiding (Monad, (>>=), return)

main:: IO()
main = do
	putStrLn "HW4.3"

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

class Monad m where   -- m :: * -> *
    return :: a -> m a                  -- return
    (>>=)  :: m a -> (a -> m b) -> m b  -- bind

instance MonadJoin m => Monad m where
	return = returnJoin
	x >>= f = f (join x)
