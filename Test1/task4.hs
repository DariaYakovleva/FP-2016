--максимально короткая реализация
--Prelude, Data.List, Data.Char, Data.Map, Data.Tree
--содержит 0..9 хотя бы раз


main = do
	let t = Node 3 (Node 1 Leaf Leaf) $ Node 123 (Node 4 Leaf Leaf) Leaf
	print $ verticalTreePrint t


data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show


class PrintTree a where
	verticalTreePrint :: Show a => Tree a -> String

instance PrintTree (Tree a) where
	verticalTreePrint Leaf =  ""

