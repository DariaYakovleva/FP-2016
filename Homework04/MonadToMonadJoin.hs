{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

import Prelude hiding (Monad, (>>=), return)
import JoinFishMonad

instance Monad m => MonadJoin m where
	returnJoin = return
	join m = m >>= id
