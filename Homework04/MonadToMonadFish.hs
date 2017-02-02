{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

import Prelude hiding (Monad, (>>=), return)
import JoinFishMonad

instance Monad m => MonadFish m where
	returnFish = return
	f >=> g = \m -> ((return m >>= f) >>= g)