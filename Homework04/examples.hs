
main = do
	print $ "f"
makePair :: Maybe a -> Maybe b -> Maybe (a, b)
--makePair a b = a >>= (b >>= (\x y -> return (x, y)))
makePair a b = a >>= \x -> b >>= \y -> return (x, y)

class Monad m where   -- m :: * -> *
    return :: a -> m a                  -- return
    (>>=)  :: m a -> (a -> m b) -> m b  -- bind

data Maybe a = Nothing | Just a

instance Monad Maybe where
    return = Just  
    Nothing >>= _ = Nothing
    Just a  >>= f = f a

data Either a b = Left a | Right b
--Either :: * -> * -> *

instance Monad (Eiter a) where
	return = Right
	Right v >>= f = f v
	Left w >>= f = Left w
-- (>>=) Either a b -> (b -> Either a c) -> Either a c

(.)   ::            (b -> c) -> (a -> b) -> a -> c
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>==>) f g x = f x >>= g
(<==<) f g x = f =<< g x

m >>= (f >=> g) ≡ m >>= f >>= g
m >>= (f <=< g) ≡ m >>= g >>= f

(f >=> g) >=> h ≡ f >=> (g >=> h)    -- associativity

safeTail :: [a] -> Maybe [a]
safeInit :: [a] -> Maybe [a]

safeStrip :: [a] -> Maybe [a]
safeStrip = safeTail >=> safeInit

instance Monad [] where
    return x = [x]
    l >>= f  = concat (map f l)  -- or using concatMap

ghci> [10, 5, 7] >>= replicate 3
[10, 10, 10, 5, 5, 5, 7, 7, 7]

--return :: a -> m a                  -- return
    --(>>=)  :: m a -> (a -> m b) -> m b  -- bind



surround :: a -> a -> [a] -> [a]
surround '(' ')' "abacaba" = "(a)(b)(a)(c)(a)(b)(a)"
surround a b lst = lst >>= (\x -> [a, x, b])


manHeaps :: (Int, Int) -> [(Int, Int)]
manHeaps (a, b) = filter isCorrectHeaps
    [ (a - 1, b    ), (a   *   2, b `div` 2)
    , (a    , b - 1), (a `div` 2, b   *   2)
    ]
  where
    isCorrectHeaps (x, y) = x >= 0 && y >= 0

zeroIn3 :: (Int, Int) -> Bool
zeroIn3 h = any (\(a, b) -> a == 0 && b == 0) $
            [h] >>= manHeaps >>= manHeaps >>= manHeaps

join :: Monad m => m (m a) -> m a

--ghci> join [[3, 4], [7, 10]]
--[3, 4, 7, 10]

join m = m >>= id

liftM    :: Monad m => (a -> b) -> m a -> m b
liftM f a = a >>= return . f
liftM2   :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f a = a >>= (\x -> b >>= (\y -> return $ f x y))

--обычный if
ifM   :: Monad m => m Bool -> m a -> m a -> m a
ifM a b c = a >>= (\x -> if x then b else c)

(||^) :: Monad m => m Bool -> m Bool -> m Bool
