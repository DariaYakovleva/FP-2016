--"1. Полное соблюдение стайл-гайда в рамках известного вам.
--2. Если требуется использовать какую-то коллекцию (мапа), то необходимо использовать библиотеки, а не свои
--(по возможности этот пункт применяется и к другим библиотекам)
--3. Запрещается использовать do-нотацию вне IO."

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

main :: IO()
main = do
	print $ "HW4.2"
	print $ [(2, 3)] >>= manHeaps
	print $ zeroInMin (12, 13)

 -- return number of steps required to set to zero both elements of pair with manHeaps function.
zeroInMin :: (Int, Int) -> Int
zeroInMin h = zeroInMin' [h]

zeroInMin' :: [(Int, Int)] -> Int
zeroInMin' h = let lst = h >>= manHeaps in 
				case any (\(a, b) -> a == 0 && b == 0) $ lst of
			  		True -> 1
			  		False -> (zeroInMin' lst) + 1

manHeaps :: (Int, Int) -> [(Int, Int)]
manHeaps (a, b) = filter isCorrectHeaps
    [ (a - 1, b    ), (a   *   2, b `div` 2)
    , (a    , b - 1), (a `div` 2, b   *   2)
    ]
  where
    isCorrectHeaps (x, y) = x >= 0 && y >= 0