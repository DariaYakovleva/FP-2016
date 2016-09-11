
-- У всех top-level функций должны быть явно указаны типы.

main = do
	print $ take 3 $ zipWith app [[1], [2]] [1,2]
	print $ take 5 $ zipN (take 3) $ repeat [0..]
--	[[0,0,0],[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
--	print $ take 5 $ zipN (take 3) $ repeat [0..1]
--	[[0,0,0],[1,1,1]]

zipN :: ([a] -> [a]) -> [[a]] -> [[a]] 
zipN f (x:[]) = [x]
zipN f (x:y:xs) = let res = map f $ zipWith mkList x y 
				  in zipN' f res xs

zipN' :: ([a] -> [a]) -> [[a]] -> [[a]] -> [[a]] 
zipN' f cur (x:xs) = let res = map f $ zipWith app cur x  
					 in zipN' f res xs

app:: [a] -> a -> [a]
app x y = x ++ [y]

mkList:: a -> a -> [a]
mkList x y = x:y:[]