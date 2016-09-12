
-- У всех top-level функций должны быть явно указаны типы.

main = do
	print $ take 5 $ zipN (take 3) $ repeat [0..]
--	[[0,0,0],[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
	print $ take 5 $ zipN (take 3) $ repeat [0..1]
--	[[0,0,0],[1,1,1]]

zipN :: ([a] -> b) -> [[a]] -> [b]
zipN f ([]:xs) = []
zipN f x = f (map head x) : zipN f (map tail x)
