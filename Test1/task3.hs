--максимально короткая реализация
--Prelude, Data.List, Data.Char, Data.Map, Data.Tree
--содержит 0..9 хотя бы раз

import Prelude hiding ((!!))

main = do
	print $ shift (Cons 1 (Cons 2 Nil))
	print $ (Cons 1 (Cons 2 Nil)) !! 1
	print $ (Nill) !! 1
	print $ shift (Nill)



data ShiftList a = Nil | Cons a (ShiftList a) deriving Show

class Index a where
	(!!) :: ShiftList a -> Int -> a
	shift :: ShiftList a -> ShiftList a
	append :: ShiftList a -> a -> ShiftList a

instance Index (ShiftList a) where
	(Cons a (xs)) !! 0 =  a
	(Cons a (xs)) !! n =  xs !! (n-1)
	shift (Cons y (xs)) = append xs y
	append Nil y = Cons y (Nil)
	append (Cons x xs) y = Cons x (append xs y)

