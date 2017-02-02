--"1. Полное соблюдение стайл-гайда в рамках известного вам.
--2. Если требуется использовать какую-то коллекцию (мапа), то необходимо использовать библиотеки, а не свои
--(по возможности этот пункт применяется и к другим библиотекам)
--3. Запрещается использовать do-нотацию вне IO."

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
--import Prelude hiding (Monad, (>>=), return)
main :: IO()
main = do
	print $ "HW4.4"
	print $ runState pop [5,8,2,1]
	print $ runState (push 3) [5,8,2,1]

data State s a = State 
	{ runState :: s -> (a, s) }

--class Monad m where   -- m :: * -> *
--    return :: a -> m a                  -- return
--    (>>=)  :: m a -> (a -> m b) -> m b  -- bind

--State type is wrapper for function that takes some «state» and returns 
--pair of «value» (result, state outcome) and «new state».
--return must create State from function that doesn't change «state» and returns given «value».
--bind applies function to «value», feeds old «state» and returns new «state» with new «value».

instance Functor (State s) where
	--fmap :: (a -> b) -> f a -> f b 
	fmap f state = State $ \s -> let (curA, curS) = runState state s in runState (return (f curA)) curS

instance Applicative (State s) where
	--pure :: a -> f a 
	pure = return
	--(<*>) :: f (a -> b) -> f a -> f b 
	f <*> state = State $ \s -> let (curA, curS) = runState state s 
								in let fun = fst (runState f s) 
								in runState (return (fun curA)) curS


instance (Applicative (State s)) => Monad (State s) where
	return a = State $ \s -> (a, s)
	curState >>= f = State $ \s -> let (curA, curS) = runState curState s in runState (f curA) curS
	

pop :: State [Int] Int  
pop = State $ \(x:xs) -> (x, xs)  
  
push :: Int -> State [Int] ()  
push a = State $ \xs -> ((), a:xs)  
