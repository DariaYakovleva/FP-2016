{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

import Prelude hiding (Monad, (>>=), return)
import JoinFishMonad

instance MonadFish m => MonadJoin m where
	returnJoin = returnFish
	join = id >=> id
