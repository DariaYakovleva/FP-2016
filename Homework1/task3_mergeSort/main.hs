import           System.Random (newStdGen, randomRs)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

main = do
	example <- randomIntList 5 (-10) 10
	print $ example
	print $ mergeSort example

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = let half = div (length x) 2
			  in merge (mergeSort (take half x)) $ mergeSort $ drop half x

merge :: [Int] -> [Int] -> [Int]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) = if x <= y then x: merge xs (y:ys) else y: merge (x:xs) ys
