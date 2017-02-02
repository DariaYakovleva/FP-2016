--TASK
--Implement mutable dynamic Array (like Java ArrayList) using ST monad.
--Your implementation should support next operations:
--pushBack, popBack, get element by index, set element by index, fromList, toList

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module MyArray where

import Control.Monad.ST
import Data.STRef
import Data.Array.ST

--main :: IO ()
--main = do
--	putStrLn $ "HW9.1"
--	mm <- stToIO $ myArr
--	stToIO $ pushBack mm 33
--	stToIO $ pushBack mm 34
--	stToIO $ pushBack mm 35
--	--stToIO $ popBack mm
--	lst <- stToIO $ toList mm
--	print $ lst

--newArray   :: Ix i => (i, i) -> e -> m (a i e)
--readArray  :: (MArray a e m, Ix i) => a i e -> i -> m e
--writeArray :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()

myArr :: ST s (MyArray s Int)
myArr = newMyArray

type MyArray s a = (STRef s Int, STRef s (STArray s Int a))

pushBack :: MyArray s a -> a -> ST s ()
pushBack lst x = do 
	ccurLst <- readSTRef (snd lst)
	maxLen <- getBounds ccurLst
	curLen <- readSTRef $ fst lst
	modifySTRef (fst lst) ((+) 1)
	if (curLen < snd maxLen) then do
		setElement lst curLen x	
	else do
		curLst <- MyArray.toList lst
		newLst <- newListArray (0, (snd maxLen) * 2) curLst
		writeSTRef (snd lst) newLst
		setElement lst curLen x

popBack :: MyArray s a -> ST s ()
popBack lst = modifySTRef (fst lst) (flip (-) 1)

newMyArray :: ST s (MyArray s Int)
newMyArray = do
	fs <- newSTRef 0
	sn' <- newArray (0, 1) 0
	sn <- newSTRef sn'
	return (fs, sn)

--get element by index
getElement :: MyArray s a -> Int -> ST s a
getElement lst ind = do
	curLst <- readSTRef (snd lst)
	readArray curLst ind

--set element by index
setElement :: MyArray s a -> Int -> a -> ST s ()
setElement lst ind value = do
	curLst <- readSTRef (snd lst)
	writeArray curLst ind value
	writeSTRef (snd lst) curLst

----fromList
fromList :: [a] -> ST s (MyArray s a)
fromList lst = do
	fs <- newSTRef $ length lst
	sn' <- newListArray (0, length lst) lst
	sn <- newSTRef sn'
	return (fs, sn)	

--toList
toList :: MyArray s a -> ST s [a]
toList lst = do
	curLst <- readSTRef (snd lst)
	curLen <- readSTRef $ fst lst
	llst <- getElems curLst
	return $ take curLen llst