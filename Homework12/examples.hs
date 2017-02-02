Comonad
class Applicative m => Monad m where
    return :: a -> m a
    (=<<)  :: (a -> m b) -> m a -> m b
    join   :: m (m a) -> m a

class Functor w => Comonad w where
    extract   :: w a -> a
    (<<=)     :: (w a -> b) -> w a -> w b  -- extend
    duplicate :: w a -> w (w a)

data [a] = [] | a : [a]
data ListZipper a = LZ [a] a [a]

listLeft, listRight :: ListZipper a -> ListZipper a
listLeft  (LZ (a:as) x bs) = LZ as a (x:bs)
listRight (LZ as x (b:bs)) = LZ (x:as) b bs

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

instance Functor ListZipper where
    fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

extract :: ListZipper a -> a
extract (LZ _ x _) = x

iterate' :: (a -> a) -> a -> [a]
iterate' f = tail . iterate f

genericMove :: (z a -> z a)
            -> (z a -> z a)
            -> z a
            -> ListZipper (z a)
genericMove a b z = LZ (iterate' a z) z (iterate' b z)

duplicate :: ListZipper a -> ListZipper (ListZipper a)
duplicate = genericMove listLeft listRight    

Env Comonad 	
data Env e a = Env e a  -- just a pair
instance Comonad (Env e) where
    extract      (Env _ a) = a
    extend f env@(Env e _) = Env e (f env)

ask  :: Env e a -> e
asks :: (e -> e') -> Env e a -> Env e' a 
