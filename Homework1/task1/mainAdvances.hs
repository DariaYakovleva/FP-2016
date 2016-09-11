
-- У всех top-level функций должны быть явно указаны типы.

advancedTests    = [ "+1", "1 +1", "-1 +1", "+1 -1"]
advancedMustFail = ["1 + 1", "+-1", "+-1", "-+1", "++1", "1+1", "+-1"]


main = do
	print $ test advancedTests
	print $ test advancedMustFail

test :: [String] -> [Int]
test [] = []
test (x:xs) = stringSum x : test xs

stringSum :: String -> Int
stringSum s = sum $ map myRead $ words s

myRead :: String -> Int
myRead ('+':s) = read s
myRead s = read s
