module Main where

--import Lib

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
--{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

-- TASK
-- написать алгоритм ‘лойда-”оршелла, использу€ IOArray.

import Data.Array.IO

main :: IO()
main = do 
	print $ "CW2.2"
	--test
	edges <- newArray (0, 2) (newArray (0, 2) 1000000000) :: IO (IOArray Int (IO (IOArray Int Int)))	
	a' <- readArray edges 0
	writeArray a' 0 0 
	writeArray edges a' 0
	a' <- readArray edges 1
	writeArray a' 1 1 
	writeArray edges a' 1
	a' <- readArray edges 2
	writeArray a' 2 2 
	writeArray edges a' 2
	a' <- readArray edges 1
	writeArray a' 2 2 
	writeArray edges a' 1
	a' <- readArray edges 2
	writeArray a' 3 1 
	writeArray edges a' 2
	floyd 3 edges


-- ‘лойд
--for (int k = 0; k < n; k++)
--	for (int i = 0; i < n; i++)
--		for (int j = 0; j < n; j++)			
--			d[i][j] = min(d[i][j], d[i][k] + d[k][j])

-- take matrix, contains edges, 1000000000 = no edge
-- i.e. 0 100 1000000000
--		1  0  1000000000
--      1  2    0		
-- return distance array
floyd :: Int -> IO (IOArray Int (IO (IOArray Int Int))) -> IO (IOArray Int (IO (IOArray Int Int)))
floyd n input = setEdges n 0 0 input	

setEdges :: Int -> Int -> Int -> Int -> IO (IOArray Int (IO (IOArray Int Int))) -> IO (IOArray Int (IO (IOArray Int Int)))
setEdges n i j k dist = do
	curDist <- dist
	if (k == n)
	then 
		return curDist
	else
		if (i == n)
		then 
			setEdges n (k + 1) 0 0 dist
		else
			if (j == n)
			then 
				setEdges n k (i + 1) 0 dist
			else
				a' <- readArray dist i
				a <- readArray a' k
				b' <- readArray dist k
				b <- readArray a' j
				c' <- readArray dist i
				c <- readArray a' j
				if c > a + b
					then
						tmp <- readArray dist i
						writeArray tmp j (a + b)
						writeArray dist tmp i
						setEdges n k i (j + 1) dist
					else
						setEdges n k i (j + 1) dist

