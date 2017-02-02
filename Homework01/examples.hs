main = caseFun 1


fun :: Int -> Int -> Int -- function example
fun x y = x * y

list :: [Int] --head, tail, last, init, drop take, replicate, zip, unzip, zipWith
list = [1, 2, 3]

list2 = list ++ list

lam = \x y -> x + y

fun2 :: Double -> Double -> Double
fun2 x y = a + b
	where 
		square s = s * s
		a = square x
		b = square y


caseFun :: Integer -> Integer
caseFun n
	| n < 0 = 1
	| otherwise = n + 1

caseFun2 :: Int -> String
caseFun2 x = case x of
				  0 -> "zero"
				  _ -> "not zero"

ff = filter (>3) [1, 2, 3]
mm = map (+2) [2, 3, 4]

f. g = \x -> f (g x)
