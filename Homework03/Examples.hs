main = do
	print $ Book 1 2
	print $ Book2 1

data Book = Book Int Int deriving Show
newtype Book2 = Book2 Int deriving Show

newtype ARFun a r = ARFun (a -> r)



newtype Coin color = Coin { getCoin :: Int }
data Blue
data Red
blue = undefined :: Blue
red  = undefined :: Red
createCoins :: color -> Int -> Coin color
createCoins _ = Coin 
c1 = createCoins blue 10
c2 = Coin 5 :: Coin Red
addCoins :: Coin color -> Coin color -> Coin color
addCoins (Coin a) (Coin b) = Coin (a + b)

-- get type :k ot :kind
