{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module JoinFishMonad where

import Prelude hiding (Monad, (>>=), return)

main:: IO()
main = do
	putStrLn "HW4.3"

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

class Monad m where   -- m :: * -> *
    return :: a -> m a                  -- return
    (>>=)  :: m a -> (a -> m b) -> m b  -- bind
