--TASK
--Compare your implementation of mutable ST array with other implementations 
--(List, DList, Data.Sequence, smth else) using criterion package.
--Think about how this implementations could be compared in the most honest way.

import Criterion.Main
import Data.List
import Data.Sequence
import Control.Monad.ST
import Control.Monad
import Data.STRef
import Data.Array.ST
import MyArray

main = defaultMain [
  bgroup "push" [ bench "MyArray" $ whnf pushToMyArray 1000
  				 , bench "List"  $ whnf pushToList 1000
  				 , bench "Sequence"  $ whnf pushToSeq 1000
               ],
  bgroup "find element by index" [ bench "MyArray" $ whnf (findXinMyArray myA) 999
  				 , bench "List"  $ whnf (findXinList myList) 999
  				 , bench "Sequence"  $ whnf (findXinSeq mySeq) 999
               ],
  bgroup "fromList" [ bench "MyArray" $ whnf MyArray.fromList myList
  				 , bench "Sequence"  $ whnf Data.Sequence.fromList myList
               ]
  --bgroup "popBack" [ bench "MyArray" $ whnf popMyArray 999
  --				 , bench "List"  $ whnf (popList myList) 999
  --				 , bench "Sequence"  $ whnf (popSeq mySeq) 999
  --             ]
  ]	

--main :: IO ()
--main = do
--	putStrLn $ "HW9.2"
--	print $ pushToList 100

myA :: ST s (MyArray s Int)
myA = newMyArray

myList :: [Int]
myList = pushToList 1000

mySeq :: Seq Int
mySeq = pushToSeq 1000

pushToMyArray :: Int -> ()
pushToMyArray n = runST $ pushToMyArray' n

pushToMyArray' :: Int -> ST s ()
pushToMyArray' 0 = return ()
pushToMyArray' n = do 
	mm <- myA
	pushBack mm n
	pushToMyArray' (n - 1)

pushToList :: Int -> [Int]
pushToList 0 = []
pushToList n = (pushToList (n - 1)) ++ [n]

pushToSeq :: Int -> Seq Int
pushToSeq 0 = empty
pushToSeq n = (pushToSeq (n - 1)) |> n

-- find elem by index x

findXinMyArray :: ST s (MyArray s Int) -> Int -> ST s Int
findXinMyArray lst ind = do
	mm <- lst
	getElement mm ind

findXinList :: [Int] -> Int -> Int
findXinList (x:xs) 0 = x
findXinList (x:xs) ind = findXinList xs (ind - 1)

findXinSeq :: Seq Int -> Int -> Int
findXinSeq = Data.Sequence.index

-- popBack a lot of times

popMyArray :: Int -> ()
popMyArray n = runST $ popMyArray' n

popMyArray' :: Int -> ST s ()
popMyArray' 0 = return ()
popMyArray' n = do 
	mm <- myA
	popBack mm
	popMyArray' (n - 1)

popList :: [Int] -> Int -> [Int]
popList xs 0 = xs
popList xs n = popList (init xs) (n - 1)

popSeq :: Seq Int -> Int -> Seq Int
popSeq xs 0 = xs
popSeq xs n = case (viewr xs) of
	EmptyR -> popSeq empty 0
	(xs :> x) -> popSeq xs (n - 1)

