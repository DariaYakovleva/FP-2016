-- love haskell

{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Examples
       ( Tree (..)  
       ) where

import Prelude hiding (Monad, (>>=), return)


class Monoid a where
        mempty  :: a        
        mappend :: a -> a -> a    
        mconcat :: [a] -> a

class Monad m where   -- m :: * -> *
    return :: a -> m a                  -- return
    (>>=)  :: m a -> (a -> m b) -> m b  -- bind

class Functor f where
	fmap :: (a -> b) -> f a -> f b 

class Applicative f where
	pure :: a -> f a 
	(<*>) :: f (a -> b) -> f a -> f b 

class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldr :: (a -> b -> b) -> b -> t a -> b
 
class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Ord)

data Either a b  =  Left a | Right b
  deriving (Eq, Ord, Read, Show)

newtype Const a b = Const { getConst :: a }

data Tree a = Leaf | Node a (Tree a) (Tree a)   

data Writer w a = Writer { runWriter :: (a, w) }
data Reader r a = Reader {  runReader :: r -> a }
data State s a = State 
	{ runState :: s -> (a, s) }
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

class MonadTrans t where    
    lift :: (Monad m) => m a -> t m a


class (Monad m) => MonadIO m where -- ??
    liftIO :: IO a -> m a

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- ST monad
data ST s a
runST :: (forall s. ST s a) -> a 
stToIO :: ST RealWorld a -> IO a  

class Monad m => MonadState s m | m -> s where
    get :: m s
    get = state (\s -> (s, s))
  
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a

get :: (Monad m) => StateT s m s
get = state $ \ s -> (s, s)

put :: (Monad m) => s -> StateT s m ()
put s = state $ \ _ -> ((), s)

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = state $ \ s -> ((), f s)

gets :: (Monad m) => (s -> a) -> StateT s m a
gets f = state $ \ s -> (f s, s)


class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

-- Lens

type Lens' s a  = Lens s s a a

set  :: Lens' s a -> a -> s -> s         -- set    value (setter)
view :: Lens' s a -> s -> a              -- lookup value (getter)
over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)

