{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

main :: IO()
main = do
	print $ "Binomial Heap"
	print $ heap1
	print $ heap4
	print $ getMinimum heap1
	print $ getMinimum heap4
	print $ insert heap4 1
	print $ getMinimum $ insert heap4 1

-- some tests
tree1 :: BinomialTree Int
tree1 = Node {key=5, degree=0, children=[]}
tree2 :: BinomialTree Int
tree2 = Node {key=2, degree=0, children=[]}
tree3 :: BinomialTree Int
tree3 = Node {key=7, degree=0, children=[]}
heap1 :: BinomialHeap Int
heap1 = [tree1]
heap2 :: BinomialHeap Int
heap2 = [tree2]
heap3 :: BinomialHeap Int
heap3 = [tree3]
heap4 :: BinomialHeap Int
heap4 = merge heap1 $ merge heap2 heap3


data BinomialTree a = Node {key :: a, degree :: Int, children :: [BinomialTree a]} deriving Show
type BinomialHeap a = [BinomialTree a]

initHeap :: a -> BinomialHeap a
initHeap x = [Node {key = x, degree = 0, children = []}]

-- O(log n)
getMinimum :: Ord a => BinomialHeap a -> Maybe a
getMinimum [] = Nothing
getMinimum heap = Just $ foldl1 min $ map key heap

-- O(log n)
merge :: Ord a => BinomialHeap a -> BinomialHeap a -> BinomialHeap a
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
	| degree x < degree y = uniqueList (x:y:(merge xs ys))
	| otherwise = uniqueList (y:x:(merge xs ys))

uniqueList :: Ord a => BinomialHeap a -> BinomialHeap a
uniqueList [] = []
uniqueList (x:[]) = [x]
uniqueList (x:y:xs)
	| degree x == degree y = uniqueList ((mergeTree x y):xs)
	| otherwise = x:(uniqueList (y:xs))

mergeTree :: Ord a => BinomialTree a -> BinomialTree a -> BinomialTree a
mergeTree x y = if key x < key y
				then Node {key = key x, degree = degree x + 1, children = children x ++ [y]} 
				else Node {key = key y, degree = degree y + 1, children = children y ++ [x]} 

-- O(log n)
insert :: Ord a => BinomialHeap a -> a -> BinomialHeap a
insert heap x = merge heap $ initHeap x


