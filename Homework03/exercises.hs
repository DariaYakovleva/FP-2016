--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldl :: (b -> a -> b) -> b -> [a] -> b

main = do
	print $ xor' [True, False, True, False, True]
	print $ numEven [[1, 2, 3], [], [2, 4], [0, 1]] -- = 4
	print $ secondMax [2, 1, 3] -- = 2

xor' :: [Bool] -> Bool
xor' (x:xs) = foldr xor x xs

xor :: Bool -> Bool -> Bool
xor x y
	| x == y = False
	| otherwise = True

numEven :: Integral a => [[a]] -> Int
numEven x = foldr even' 0 x

even' :: Integral a => [a] -> Int -> Int
even' [] c = c
even' (x:xs) c
	| even x = c + 1 + (even' xs 0)
	| otherwise = c + (even' xs 0)

secondMax :: Ord a => [a] -> a
secondMax (x:y:xs)
	| x > y = snd $ foldr smax (x, y) xs
	| otherwise = snd $ foldr smax (y, x) xs

smax :: Ord a => a -> (a, a) -> (a, a)
smax p (x, y)
	| p > x = (p, x)
	| p > y = (x, p)
	| otherwise = (x, y)