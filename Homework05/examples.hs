class Functor f where               -- f :: * -> *
    fmap :: (a -> b) -> f a -> f b

    (<$) :: a -> f b -> f a

fmap getPostTitle (findPost 1)
--$ такой же что и fmap
infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

getPostTitle <$> findPost 1

instance Functor [] where
    fmap = map

ghci> fmap (*2) [1..3]
[2,4,6]
ghci> map (*2) [1..3]
[2,4,6]

ghci> :k (->)
(->) :: * -> * -> *
ghci> :k (->) Int
(->) Int :: * -> *

instance Functor ((->) r)  where
    fmap = (.)

ghci> let foo = fmap (+3) (+2)
ghci> foo 10
15

class Bifunctor p where
    bimap  :: (a -> b) -> (c -> d) -> p a c -> p b d
    first  :: (a -> b)             -> p a c -> p b c
    second ::             (b -> c) -> p a b -> p a c

instance Bifunctor (,) where
    bimap f g (a, b) = (f a, g b)

instance Bifunctor Either where
    bimap f _ (Left a)  = Left  (f a)
    bimap _ g (Right b) = Right (g b)

ghci> first (+1) (1, 2)
(2,2)
ghci> bimap (+1) (+2) (1, 2)
(2,4)


-- functor laws
1. fmap id = id
2. fmap (f . g)   = fmap f . fmap g
   fmap (f . g) F = fmap f (fmap g F)

-- bifunctor laws
1. bimap  id id ≡ id
   first  id    ≡ id
   second    id ≡ id

2. bimap f g ≡ first f . second g

3. bimap  (f . g) (h . i) ≡ bimap  f h . bimap  g i
   first  (f . g)         ≡ first  f   . first  g
   second         (h . i) ≡ second   h . second   i


class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

    (*>) :: f a -> f b -> f b
    (<*) :: f a -> f b -> f a

f a *> f b = const id
a *> b = (\x y -> y) <$> a <*> b
a <* b = (\x y -> x) <$> a <*> b

instance Applicative Maybe where
    pure = Just
    Nothing <*> _         = Nothing
    Just f  <*> something = fmap f something

ghci> Just (+3) <*> Just 9
Just 12
ghci> pure (+3) <*> Just 10
Just 13
ghci> Just (++"hahah") <*> Nothing
Nothing
?ghci> pure (++"what") <*> pure "lol"

ghci> Just (+3) <*> Just 9
Just 12
ghci> pure (+3) <*> Just 10
Just 13
ghci> Just (++"hahah") <*> Nothing
Nothing
ghci> pure (++"what") <*> pure "lol"
"lolwhat"


instance Applicative [] where
    pure x    = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]
[16,20,22,40,50,55,80,100,110]

ghci> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]

ghci> [(*2), (+3)] <*> [1, 2, 3]
[2, 5]

newtype ZipList a = ZipList { getZipList :: [a] }
                  
instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)

ghci> ZipList [2, 3]
ZipList {getZipList = [2,3]}

ghci> (*) <$> ZipList [2, 5, 10] <*> ZipList [8, 10, 11]
ZipList {getZipList = [16,50,110]}


hci> (pure 3) "blah"
3

instance Applicative ((->) r) where
    pure x  =
    f <*> g =

instance Applicative ((->) r) where
    pure x  = \_ -> x
    f <*> g = \x -> f x (g x)



Applicative laws

1. identity
   pure id <*> v = v

2. composition
   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

3. homomorphism
   pure f <*> pure x = pure (f x)

4. interchange
   u <*> pure y = pure ($ y) <*> u


ghci> (*) <$> Just 5 <*> Just 3
Just 15
ghci> liftA2 (*) (Just 5) (Just 3)
Just 15
ghci> :t liftA3
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d

ghci> import Data.Char (isUpper, isDigit)
ghci> import Control.Applicative (liftA2)
ghci> let isUpperOrDigit = liftA2 (||) isUpper isDigit
ghci> :t isUpperOrDigit 
isUpperOrDigit :: Char -> Bool
ghci> isUpperOrDigit 'A'
True
ghci> isUpperOrDigit '3'
True
ghci> isUpperOrDigit 'a'
False

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

ghci> Nothing <|> Just 3 <|> empty <|> Just 5
Just 3

instance Alternative [] where
    empty = []
    (<|>) = (++)

ghci> [] <|> [1,2,3] <|> [4]
[1,2,3,4]

guard           :: (Alternative f) => Bool -> f ()
guard True      =  pure ()
guard False     =  empty

evenMPair :: (Integral a, Integral b) => Maybe a -> Maybe b -> Maybe (a, b)
evenMPair a b = a >>= \x ->
                b >>= \y ->
                if even x && even y
                then Just (x, y)
                else Nothing

evenPair :: (Alternative m, Monad m, Integral a, Integral b) 
         => m a -> m b -> m (a, b)
evenPair a b = a >>= \x ->
               b >>= \y ->
               guard (even x && even y) >>
               return (x, y)

ghci> evenPair (Just 2) (Just 4)
Just (2,4)
ghci> evenPair (Just 2) (Just 5)
Nothing

sweetPythags = [(x,y,z) | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2]
ghci> take 5 sweetPythags 
[(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17)]
pythagsWithoutSugar =
  [1..]  >>= \z ->
  [1..z] >>= \x ->
  [x..z] >>= \y ->
  guard (x^2 + y^2 == z^2) >>
  return (x, y, z)


ghci> take 5 pythagsWithoutSugar 
[(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17)]

ghci> evenPair [1..5] [5..10]
[(2,6),(2,8),(2,10),(4,6),(4,8),(4,10)]

class (Functor t, Foldable t) => Traversable t where
    traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA :: Applicative f => t (f a) -> f (t a)

ghci> let half x = if even x then Just (x `div` 2) else Nothing
ghci> traverse half [2,4..10]
Just [1,2,3,4,5]
ghci> traverse half [1..10]
Nothing

ghci> let rep x = replicate x x
ghci> traverse rep [1..3]
[[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]

instance Traversable Maybe where
    traverse _ Nothing  = pure Nothing
    traverse f (Just x) = Just <$> f x

instance Traversable [] where
    traverse f = foldr consF (pure [])
    where 
       consF x ys = (:) <$> f x <*> ys

instance Traversable [] where
    traverse f l =

There exist Bifoldable and Bitraversable as well as Bifunctor.

