{-# OPTIONS_GHC -fno-warn-tabs #-}
--"1. Ваш код должен быть оформлен как проект и собираться при помощи stack.
--2. В вашем коде не должно быть замечаний hlint.
--3. В вашем коде не должно быть warning'ов.
--4. Использование stylish-haskell обязательно.
--5. Реализации должны быть эффективными по возможности."
--Implement Monoid & Num instances for Coin datatype from lecture.
--Implement custom lexicographical comparator for Coin datatype with respect to color, 
--i.e. if we assume that Blue < Red then c1 < c2.
--Your implementation must me abstract and work for any defined color. Think about type classes and type hierarchy.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where


main :: IO ()
main = do
	print $ myCmp c1 c2
	print $ myCmp c1 c1
	print $ myCmp c1 c3
	print $ myCmp c2 c3
	print $ c1 + c2
--	print $ c2 * c3

newtype Coin color = Coin { getCoin :: Int} deriving Show
data Blue
data Red
blue = undefined :: Blue
red  = undefined :: Red
createCoins :: color -> Int -> Coin color
createCoins _ = Coin 
c1 = createCoins blue 10
c2 = createCoins blue 5
c3 = createCoins red 10
c4 = createCoins red 1
--class Monoid m where
--    mempty  :: m
--    mappend :: m -> m -> m    
--    mconcat :: [m] -> m

instance Monoid (Coin color) where
    mempty = Coin 0
    mappend x y = Coin (getCoin x + getCoin y)
    mconcat (x:xs) = undefined

--class  Num a  where
--    {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
--    (+), (-), (*)       :: a -> a -> a
--    negate              :: a -> a
--    abs                 :: a -> a
--    signum              :: a -> a   
--    fromInteger         :: Integer -> a
--    {-# INLINE (-) #-}
--    {-# INLINE negate #-}
--    x - y               = x + negate y
--    negate x            = 0 - x

instance Num (Coin color) where
	(Coin a) + (Coin b) = Coin (a + b)
	(Coin a) - (Coin b) = Coin (a - b)
	(Coin a) * (Coin b) = Coin (a * b)
	abs (Coin a) = Coin (abs a)
	signum (Coin a) = Coin (signum a)
	fromInteger x = Coin (fromInteger x)


class Color a where
	weight :: a -> Int

instance Color Red where
	weight _ = 1
instance Color Blue where
	weight _ = 2


getColor :: Coin color -> color
getColor _ = undefined

instance (Color color) => Eq (Coin color) where
	x == y = weight (getColor x) == weight (getColor y) && (compare (getCoin x) (getCoin y) == EQ)

instance (Color color, Eq (Coin color)) => Ord (Coin color) where
	compare x y
		| weight (getColor x) < weight (getColor y) = LT
		| weight (getColor x) > weight (getColor y) = GT
		| otherwise = compare (getCoin x) (getCoin y)

myCmp :: (Color a, Color b) => Coin a -> Coin b -> Ordering
myCmp x y
	| weight (getColor x) < weight (getColor y) = LT
	| weight (getColor x) > weight (getColor y) = GT
	| otherwise = compare (getCoin x) (getCoin y)
