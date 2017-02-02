{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

import Prelude hiding (Monad, (>>=), return)
import JoinFishMonad

instance MonadFish m => Monad m where
	return = returnFish
	x >>= f = (id >=> f) x