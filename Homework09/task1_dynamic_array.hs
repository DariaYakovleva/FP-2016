--TASK
--Implement mutable dynamic Array (like Java ArrayList) using ST monad.
--Your implementation should support next operations:
--pushBack, popBack, get element by index, set element by index, fromList, toList

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Control.Monad.ST
import Data.STRef
import Data.Foldable

main :: IO()
main = do
	putStrLn $ "HW9.1"
	--print $ 

--data ST s a  -- The strict state-transformer monad
--runState :: State s a -> s -> (a, s)  -- use evalState with state to get result
--runST    :: (forall s. ST s a) -> a   -- just use runST!
--data STRef s a  -- a mutable variable
--newSTRef    :: a -> ST s (STRef s a) 
--readSTRef   :: STRef s a -> ST s a
--writeSTRef  :: STRef s a -> a -> ST s ()
--modifySTRef :: STRef s a -> (a -> a) -> ST s () 
sumST :: Num a => [a] -> a
sumST xs = runST $ do
    n <- newSTRef 0
    for_ xs $ \x ->
        modifySTRef n (+x) 
    readSTRef n

getList :: ST s (MyList s a) -> [a]
getList st = do
	tmp <- st
	readSTRef tmp	


list :: ST s (MyList s a)
list = newSTRef []

--type Len = Int
type MyList s a = STRef s [a] -- STRef Len [a]
pushBack :: a -> ST s (MyList s a) -> ST s ()
pushBack x lst = do
	     lst2 <- lst
	     modifySTRef lst2 ((:) x)
--popBack
--get element by index
--set element by index
--fromList
--toList
