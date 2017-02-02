module MyTree where


import TreePrinters

main = do
	let t = Node 3 (Node 1 Leaf Leaf) $ Node 123 (Node 4 Leaf Leaf) Leaf
	let em = Leaf
	putStr $ verticalPrint $ insert t 11
	print $ toList t

find :: (Ord a) => Tree a -> a -> Tree a
find Leaf x = Leaf
find (Node val l r) x = case (compare x val) of
	EQ -> Node val l r
	LT -> find l x
	otherwise -> find r x

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf x = Node x Leaf Leaf
insert (Node val l r) x = case (compare x val) of
	LT -> Node val (insert l x) r 
	otherwise -> Node val l (insert r x) 

delete :: (Ord a) => Tree a -> a -> Tree a
delete Leaf x = Leaf
delete (Node val l r) x = case (compare x val) of
	EQ -> erase (Node val l r)
	LT -> Node val (delete l x) r
	otherwise -> Node val l (delete r x)

erase :: (Ord a) => Tree a -> Tree a
erase (Node val Leaf Leaf) = Leaf
erase (Node val l Leaf) = l
erase (Node val Leaf r) = r
erase (Node val l (Node rval Leaf rr)) = Node rval l rr
erase (Node val l r) = Node (getMin r) l (delete r (getMin r))

getMin :: (Ord a) => Tree a -> a
getMin (Node val Leaf _) = val
getMin (Node val l r) = getMin l

--improve
toList :: (Ord a) => Tree a -> [a]
toList Leaf = []
toList t = [getMin t] ++ (toList (delete t (getMin t)))

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Leaf
fromList (x:xs) = insert (fromList xs) x

