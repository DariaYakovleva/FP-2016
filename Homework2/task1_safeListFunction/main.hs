main = do
	print $ safeTail [1, 2, 3]
	print $ safeTail ([] :: [Int])
	print $ safeInit [1, 2, 3]
	print $ safeInit ([] :: [Int])
	print $ strip [1, 2, 3]
	print $ strip ([] :: [Int])

safeTail :: [a] -> Maybe [a] 
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:y:[]) = Just $ x:[]
safeInit (x:xs) = case (safeInit xs) of 
				  Just lst -> Just $ x:lst
				  Nothing -> Just $ x:[]

strip :: [a] -> [a]
strip x = case (safeTail x) of
		  Nothing -> []
		  Just lst -> case (safeInit lst) of
		  			  Nothing -> []
		  			  Just lst -> lst