
-- У всех top-level функций должны быть явно указаны типы.

main = do
	print $ zipN sum [ [1, 2, 3] 
					 , [4, 5, 6] 
					 , [7, 8, 9]]
--					   [12,15,18]

zipN :: ([a] -> a) -> [[a]] -> [a] 
zipN f (x:[]) = x
zipN f (x:xs) = map f $ zipWith mkList x $ zipN f xs

mkList:: a -> a -> [a]
mkList x y = x:y:[]
