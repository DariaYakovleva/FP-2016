--максимально короткая реализация
--Prelude, Data.List, Data.Char, Data.Map, Data.Tree
--все подмножества длины n для заданного списка

main = do
	print $ subsets [1, 2, 3, 4] 2
	print $ subsets [1, 2, 3, 4, 5, 7] 3
	print $ subsets [4, 3, 2, 4] 4
	print $ subsets [4, 1, 3, 7] 0
	print $ subsets [2, 3, 4] 5

subsets :: [a] -> Int -> [[a]]
subsets [] n = [[]]
subsets (x:xs) n = filter ((==n) . length) $ (map (x:) (subsets xs (n - 1))) ++ (subsets xs n)
