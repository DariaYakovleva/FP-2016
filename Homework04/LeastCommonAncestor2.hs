--"1. Полное соблюдение стайл-гайда в рамках известного вам.
--2. Если требуется использовать какую-то коллекцию (мапа), то необходимо использовать библиотеки, а не свои
--(по возможности этот пункт применяется и к другим библиотекам)
--3. Запрещается использовать do-нотацию вне IO."
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

main :: IO()
main = do
	print $ "HW4.1"
	print $ leastCommonAncestor tree1 tree2



data InNode a = Node 
	{ label :: a
	, parent :: Maybe (InNode a) 
	} deriving Show

tree1 :: InNode Integer
tree1 = Node { label=1, parent=Just (Node {label=2, parent=Nothing })}
tree2 :: InNode Integer
tree2 = Node { label=3, parent=Just (Node {label=2, parent=Nothing })}

leastCommonAncestor :: Eq a => InNode a -> InNode a -> Maybe (InNode a)
leastCommonAncestor x y = firstDifferent (getPath (Just x)) (getPath (Just y)) Nothing


getPath :: Eq a => Maybe (InNode a) -> [InNode a]
getPath Nothing = []
getPath (Just x) = (getPath (parent x)) ++ [x]


firstDifferent :: Eq a => [InNode a] -> [InNode a] -> Maybe (InNode a) -> Maybe (InNode a)
firstDifferent [] _ _ = Nothing
firstDifferent _ [] _ = Nothing
firstDifferent (x:xs) (y:ys) prev = if (not (label x == label y)) then prev 
									else firstDifferent xs ys (Just x)

