main = do
	print $ safeTail [1, 2, 3]
	print $ safeTail ([] :: [Int])
	print $ safeInit [1, 2, 3]
	print $ safeInit ([] :: [Int])
	print $ strip [1, 2, 3]
	print $ strip ([] :: [Int])

safeTail :: [a] -> Either String [a]
safeTail [] = Left "empty list"
safeTail (x:xs) = Right xs

safeInit :: [a] -> Either String [a]
safeInit [] = Left "empty list"
safeInit x = Right $ init x

strip :: [a] -> [a]
strip x = case (safeTail x) of
		  Left a -> error a
		  Right lst -> case (safeInit lst) of
		  			  Left a -> error a
		  			  Right lst -> lst