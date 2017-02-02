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



data InNode a = Node { label :: a, parent :: Maybe (InNode a) } deriving Show

tree1 :: InNode Integer
tree1 = Node {label=1, parent=Just (Node {label=2, parent=Nothing})}
tree2 :: InNode Integer
tree2 = Node {label=3, parent=Just (Node {label=2, parent=Nothing})}

leastCommonAncestor :: Eq a => InNode a -> InNode a -> Maybe (InNode a)
leastCommonAncestor x y = 
	if isOnPath x y 
	then Just x
	else parent x >>= (\prev -> leastCommonAncestor prev y)


isOnPath :: Eq a => InNode a -> InNode a -> Bool
isOnPath x tree = 
	if label x == label tree 
	then True 
	else -- parent tree >>= (\prev -> isOnPath x prev)
		case parent tree of
			Nothing -> False
			Just prev -> isOnPath x prev
