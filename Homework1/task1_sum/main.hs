
-- У всех top-level функций должны быть явно указаны типы.

tests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030"
        , " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 "
        , "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"
        ]

mustFail = ["1+", "+1", "--2", "1.2", "1-1", "asd"]

main = do
--	test2 mustFail
	print $ test tests
--	print $ test mustFail


stringSum :: String -> Int
stringSum s = sum $ map read $ words s

test :: [String] -> [Int]
test [] = []
test (x:xs) = stringSum x : test xs

test2 :: [String] -> IO()
test2 [] = print "END"
test2 (x:xs) = do
	print $ stringSum x
	test2 xs
