-- TASK
--2 задания: (1) полутеоретическое на монады и классы вокруг них и (2) на трансформеры монад
--Implement all functions with your Tree data type in terms of Writer monad. 
--	So you can observe evaluation log of each function call if necessary.
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
--{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

import Control.Monad.Writer

main :: IO()
main = do
	print $ "HW6.1"
	print $ runWriter (half 8)
	print $ runWriter $ half 8 >>= half
	print $ runWriter $ find 123 t1
	print $ runWriter $ (insert 1234 t1) >>= (delete 1234)

--data Writer w a = Writer { runWriter :: (a, w) }

half :: Integer -> Writer [Char] Integer
half x = writer $ (x `div` 2, "I have x")

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

t1 :: Tree Int
t1 = Node 3 (Node 1 Leaf Leaf) $ Node 123 (Node 4 Leaf Leaf) Leaf

find :: (Ord a) => a -> Tree a -> Writer String (Tree a)
find _ Leaf = writer $ (Leaf, "empty tree; ")
find x (Node val l r) = case (compare x val) of
	EQ -> writer $ (Node val l r, "element found; ")
	LT -> find x l
	GT -> find x r

insert :: (Ord a) => a -> Tree a -> Writer String (Tree a)
insert x Leaf = writer $ (Node x Leaf Leaf, "insert to empty tree; ")
insert x (Node val l r) = case (compare x val) of
	LT -> let (tree, logs) = runWriter (insert x l) in writer (Node val tree r,  "insert left; " ++ logs)
	_ -> let (tree, logs) = runWriter (insert x r) in writer (Node val l tree, "insert right; " ++ logs)

delete :: (Ord a) => a -> Tree a -> Writer String (Tree a)
delete _ Leaf = writer $ (Leaf, "empty tree; ")
delete x (Node val l r) = case (compare x val) of
	EQ -> writer $ (erase (Node val l r), "delete element; ")
	LT -> let (tree, logs) = runWriter (delete x l) in writer (Node val tree r,  "go delete left; " ++ logs) 
	_ -> let (tree, logs) = runWriter (delete x r) in writer (Node val l tree,  "go delete right; " ++ logs) 

erase :: (Ord a) => Tree a -> Tree a
erase (Node _ Leaf Leaf) = Leaf
erase (Node _ l Leaf) = l
erase (Node _ Leaf r) = r
erase (Node _ l (Node rval Leaf rr)) = Node rval l rr
erase (Node _ l r) = let (tree, _) = runWriter (delete (getMin r) r) in Node (getMin r) l tree

getMin :: (Ord a) => Tree a -> a
getMin (Node val Leaf _) = val
getMin (Node _ l _) = getMin l
