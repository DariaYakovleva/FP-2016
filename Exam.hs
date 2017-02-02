-- love haskell
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Examples
       ( Tree (..)  
       ) where

import Prelude hiding (Monad, (>>=), return)

1.
Краткий список удачных конструкций Haskell 
• Замыкания (closures) – их трудно полноценно реализовать в языке без сборки мусора типа C++ (upward funarg problem)
• Каррирование (currying) – то же, что для замыканий плюс проблемы с перегрузкой функций по числу аргументов, 
как принято во многих императивных языках
• Вывод типов (type inference) – глобальный вывод типов накладывает серьезные ограничения на систему типов; 
так, наличие наследования резко ослабляет возможности вывода типов
• Сопоставление с образцом (pattern matching)
• Классы типов (type classes)

# functions
words :: String -> [String] -- сплит по пробелу
read :: Read a => String -> a
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

2.
# list
l = [2, 1, 3]
init :: [a] -> [a] -- возвращает все, кроме последнего
drop 2 l == [3]
take 1 l == [2]
replicate 3 l == [[2,1,3], [2,1,3], [2,1,3]]
zip [1,2,3] "abc" == [(1, 'a'), (2, 'b'), (3, 'c')]
unzip [(5, True), (10, False)] == ([5, 10], [True, False])
zipWith max [10, 5, 1] [2, 1, 3] == [10, 5, 3]
x = l !! i   -- x == l[i], O(i) time

# lambda
squares = map (\x -> x * x) l

# let
z = let l = [2, 1, 3]
        h = head l
    in h + 10

# where
pythagoras a b = a2 + b2
  where
    square x = x ^ 2
    a2       = square a
    b2       = square b

# if 
collatzSum n
    | n < 0     = 0
    | n == 1    = 1
    | even n    = n + collatzSum (n `div` 2)
    | otherwise = n + collatzSum (3 * n + 1)

# case
collatzSum n
    | n < 0     = 0
    | n == 1    = 1
    | even n    = n + collatzSum (n `div` 2)
    | otherwise = n + collatzSum (3 * n + 1)

map    :: (a -> b)      -> [a] -> [b]
filter :: (a -> Bool)   -> [a] -> [a]
foldr1 :: (a -> a -> a) -> [a] ->  a
span   :: (a -> Bool)   -> [a] -> ([a], [a])
uncurry :: (a -> b -> c) -> (a, b) -> c
  uncurry f (x, y) = f x y        
flip :: (a -> b -> c) -> b -> a -> c
  flip f b a = f a b
(.) :: (b -> c) -> (a -> b) -> a -> c -- function composition
  f . g = \x -> f (g x)
($) :: (a -> b) -> a -> b  -- function application
  f $ x = f x  

f x y == (f x) y
f :: a -> b -> c == f :: (a -> (b -> c))
f. g . h $ j x == (f . (g . h)) $ (j x)

3.
# types 
data Color = Red | Green | Blue
type String = [Char]
data User = User Int String String -- есть конструктор
  newtype Message = Message String -- нет конструктора
data Vector a = Vector2D a a | Vector3D a a a
data Maybe a = Nothing | Just a  -- implemented in Prelude
data Either a b  =  Left a | Right b
  deriving (Eq, Ord, Read, Show)
eitherSecond :: [a] -> Either String a
eitherSecond (_:x:_) = Right x
eitherSecond _       = Left "list doesn't have second element"
data List a = Nil | Cons a (List a)
data User = User 
    { uid      :: Int
    , login    :: String
    , password :: String 
    }

# kinds
Int :: *
Maybe :: * -> *
Maybe String :: *
data MapTree k v 
  = Leaf 
  | Node k v (MapTree k v) (MapTree k v)
MapTree          :: * -> * -> *
MapTree a        :: * -> *
MapTree String v :: *

data [] a = [] | a : [a]  -- Defined in ‘GHC.Types’
[] :: * -> *  -- kind of list
[] Int :: *   -- `[] a` is the same as [a]
(->) :: * -> * -> *    -- kind of function

foldr :: (a -> b -> b) -> b -> [a] -> b -- сначала с первым и рекурсивно дальше
foldl :: (b -> a -> b) -> b -> [a] -> b -- сначала с последним и возвращает результат

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
liftM    :: Monad m => (a -> b) -> m a -> m b
liftM2   :: Monad m => (a -> b -> c) -> m a -> m b -> m c

# classes

class Eq a where  
    (==) :: a -> a -> Bool  
    (/=) :: a -> a -> Bool

class Monoid a where
        mempty  :: a        
        mappend :: a -> a -> a    
        mconcat :: [a] -> a

class Monad m where   -- m :: * -> *
    return :: a -> m a                  -- return
    (>>=)  :: m a -> (a -> m b) -> m b  -- bind

    (>>)   :: m a -> m b -> m b         -- then*
instance Monad Maybe where
    return = Just  
    Nothing >>= _ = Nothing
    Just a  >>= f = f a

6.
class Functor f where
	fmap :: (a -> b) -> f a -> f b 

  (<$>) :: Functor f => (a -> b) -> f a -> f b --*
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap _ Nothing  = Nothing
# Functor laws
1. fmap id = id
2. fmap (f . g)   = fmap f . fmap g
   fmap (f . g) F = fmap f (fmap g F)

class Functor f => Applicative f where
	pure :: a -> f a 
	(<*>) :: f (a -> b) -> f a -> f b
instance Applicative Maybe where
    pure = Just
    Nothing <*> _         = Nothing
    Just f  <*> something = fmap f something  
# Applicative laws
1. identity
   pure id <*> v = v
2. composition
   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
3. homomorphism
   pure f <*> pure x = pure (f x)
4. interchange
   u <*> pure y = pure ($ y) <*> u

class Foldable t where
  {-# MINIMAL foldMap | foldr #-}
    fold    :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldr :: (a -> b -> b) -> b -> t a -> b  -- сначала с первым и рекурсивно дальше
 
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l    

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
instance Traversable Maybe where
    traverse _ Nothing  = pure Nothing
    traverse f (Just x) = Just <$> (f x)

5.
newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Ord)

newtype Const a b = Const { getConst :: a }

data Tree a = Leaf | Node a (Tree a) (Tree a)   

7.
data Writer w a = Writer { runWriter :: (a, w) } -- (текущее значение; логи)
instance Monoid w => Monad (Writer w) where
    return a            = Writer (a, mempty)
    Writer (x, v) >>= f = let Writer (y, v') = f x 
                          in Writer (y, v `mappend` v')

data Reader r a = Reader {  runReader :: r -> a } -- принимает аргумент и обновляет значение внутри
ask   :: Reader e e
asks  :: (e -> a) -> Reader e a
local :: (e -> b) -> Reader b a -> Reader e a

instance Monad (Reader e) where
    return a = Reader $ \_ -> a
    m >>= f  = Reader $ \r -> runReader (f $ runReader m r) r

data State s a = State { runState :: s -> (a, s) } -- принимает аргумент, сохраняет значение, хранит логи
instance Monad (State s) where
    return a       = State $ \s -> (a, s)
    oldState >>= f = State $ \s -> let (a, newState) = runState oldState s
                                   in runState (f a) newState
# пример state - stack
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((), x:xs)

stackOps :: State Stack Int
stackOps = pop >>= \x -> push 5 >> push 10 >> return x

newtype Cont r a = Cont { runCont :: (a -> r) -> r }
cont :: ((a -> r) -> r) -> Cont r a
instance Monad (Cont r) where
    return a       = Cont ($ a)
    Cont arr >>= f = Cont $ \br -> arr $ \a -> runCont (f a) br

class MonadTrans t where    
    lift :: (Monad m) => m a -> t m a


class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- ST monad
data ST s a
runST :: (forall s. ST s a) -> a 
stToIO :: ST RealWorld a -> IO a  

class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

class Monad m => MonadState s m | m -> s where
    get :: m s
    get = state (\s -> (s, s))
  
    put :: s -> m ()
    put s = state (\_ -> ((), s))

    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get
      let ~(a, s') = f s
      put s'
      return a

get :: (Monad m) => StateT s m s
get = state $ \ s -> (s, s)

put :: (Monad m) => s -> StateT s m ()
put s = state $ \ _ -> ((), s)

modify :: (Monad m) => (s -> s) -> StateT s m ()
modify f = state $ \ s -> ((), f s)

gets :: (Monad m) => (s -> a) -> StateT s m a
gets f = state $ \ s -> (f s, s)


class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

class Monad m => MonadCont m where
    callCC :: ((a -> m b) -> m a) -> m a  -- call-with-current-continuation

instance MonadCont (Cont r) where
    callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
    callCC f = cont $ \c -> runCont (f (\x -> cont $ \_ -> c x)) c

8. IO
{-# LANGUAGE MagicHash #-} # для оптимизации ???
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

# do notation, >>
(>>) :: IO a -> IO b -> IO b

(action1 >> action2) world0 =
   let (_, world1) = action1 world0
       (b, world2) = action2 world1
   in (b, world2)

putStrLn :: String -> IO ()
main = do putStrLn "What is your name?"
          putStrLn "How old are you?"
          putStrLn "Nice day!"
main = (putStrLn "What is your name?") >>
       (putStrLn "How old are you?")   >>
       (putStrLn "Nice day!")

# do, <-
(>>=) :: IO a -> (a -> RealWorld -> (b, RealWorld)) -> IO b
(action1 >>= action2) world0 =
   let (a, world1) = action1 world0
       (b, world2) = action2 a world1
   in (b, world2)

getLine :: IO String
main = do s <- getLine
          putStrLn s
main = getLine >>= \s -> putStrLn s

# example!
main = do putStr "What is your name?"
          a <- readLn
          putStr "How old are you?"
          b <- readLn
          print (a,b)
        ==          
main =    putStr "What is your name?"
       >> readLn
       >>= \a -> putStr "How old are you?"
       >> readLn
       >>= \b -> print (a,b)

# ??
class EffectMonad m where -- нафиг??
    return :: a -> m e a
    (>>=)  :: m i a -> (a -> m j b) -> m (i <> j) b       

# изменяемые данные IORef
import Data.Array.IO
main = do arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
          a   <- readArray arr 1
          writeArray arr 1 64
          b   <- readArray arr 1
          print (a, b)

# exceptions
throwIO :: Exception e => e -> IO a
catch :: Exception e => IO a -> (e -> IO a) -> IO a          

--- что-то неинтересное???

# text
import qualified Data.Text as T
-- From pack
myTStr1 :: T.Text
myTStr1 = T.pack ("foo" :: String)

# ByteString
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8
-- From pack
bstr1 :: S.ByteString
bstr1 = S.pack ("foo" :: String)
-- From overloaded string literal.
bstr2 :: S8.ByteString
bstr2 = "bar"

# из дз interactive
type MMap = StateT (M.Map T.Text T.Text) IO
update :: T.Text -> MMap ()
update a = do
    myMap <- get
    liftIO $ putStrLn $ "input new value for '" ++ T.unpack a ++ "' property (previous: '" ++ show (M.lookup a myMap) ++ "')"
    b <- liftIO $ getLine   
    modify (M.insert a (T.pack b))
    liftIO $ putStrLn $ "property " ++ T.unpack a ++ " with value " ++ b ++ " updated"    
    config


9.
Reader has ask but doesn't have put
State has put but doesn't have ask
# Transformers!
foo :: ReaderT Int (State [Int]) Int  -- or StateT [Int] (Reader Int) Int
foo i = do
    baseCounter <- ask
    let newCounter = baseCounter + i
    put [baseCounter, newCounter]
    return newCounter
newtype MaybeIO a = MaybeIO {
   runMaybeIO :: IO (Maybe a)
}

instance Monad MaybeIO where
    return x = MaybeIO (return (Just x))
    MaybeIO action >>= f = MaybeIO $ do
        result <- action
        case result of
            Nothing -> return Nothing
            Just x  -> runMaybeIO (f x)

# MaybeT Transformer
newtype MaybeT m a = MaybeT {
   runMaybeT :: m (Maybe a)
}

instance Monad m => Monad (MaybeT m) where
    return x = MaybeT (return (Just x))
    MaybeT action >>= f = MaybeT $ do
        result <- action
        case result of
            Nothing -> return Nothing
            Just x  -> runMaybeT (f x)

transformToMaybeT :: Monad m => m a -> MaybeT m a
transformToMaybeT action = MaybeT $ do
    result <- action
    return (Just result)                

transformToMaybeT  :: Monad m => m a -> MaybeT    m a
transformToEitherT :: Monad m => m a -> EitherT l m a

class MonadTrans t where    -- t :: (* -> *) -> * -> *
    lift :: Monad m => m a -> t m a
instance MonadTrans MaybeT where
    lift = transformToMaybeT

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
instance (Monad m) => Monad (ReaderT r m) where
    return  = lift . return
    m >>= f = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (f a) r

instance MonadTrans ReaderT where
   lift m = ReaderT (const m)    -- lift ma = ReaderT $ \r -> ma

class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

-- | The CoroutineT monad is just ContT stacked with 
-- a StateT containing the suspended coroutines.
newtype CoroutineT r m a = CoroutineT 
    { runCoroutineT' :: ContT r (StateT [CoroutineT r m ()] m) a
    } deriving (Functor, Applicative, Monad, MonadCont, MonadIO)               

10. speed
import Data.List (foldl')
main :: IO ()
main = print $ foldl' (+) 0 [1..10^7] -- 10^8 ok
ламбда-выражение:
* в нормальной форме = нет бета-редукций (применения или подстановки)
* в головной нормальной форме = применение без бета-редукции или лямбда с телом в головной нф
* слабая головная н.ф. = головная или абстракция??

-- ну такое, про скорость и изменяемые массивы...

11. template haskell
-- давайте запилим функции, которые исполняются на стадии компиляции
{-# LANGUAGE TemplateHaskell #-}
module FstN where
import Language.Haskell.TH

fstN :: Int -> Q Exp
fstN n = do
   x <- newName "x"
   return $ LamE [TupP $ VarP x : replicate (n - 1) WildP] (VarE x)
ghci> runQ [| \(x,_,_) -> x |]
  LamE [TupP [VarP x_1,WildP,WildP]] (VarE x_1)   

# TH quotes
Expression quotes
    [| \x -> x + 1 |] :: Q Exp
Type quotes
    [t| Int -> Int |] :: Q Type
Pattern quotes
    [p| xs@(x:r) |] :: Q Pat
Declaration quotes
    [d| data Pair a = Pait a a |] :: Q [Dec]

# Lens -- давайте сделаем нормальные конструкторы

-- вот это больно
setHouse person value =
  person { address = (address person) { house = value }  }
setStreet person value =
  person { address = (address person) { street = value }  }

newPerson1 = setHouse person 45
newPerson2 = setStreet person "New Street"
-- решение

data Lens' obj field = Lens' 
    { view'   :: obj -> field
    , update' :: (field -> field) -> obj -> obj
    }

set' :: Lens' obj field -> field -> obj -> obj
set' ln newValue thing = (update' ln) (\_ -> newValue) thing

# examples
addressLens :: Lens' Person Address
addressLens = Lens' address (\fn thing -> thing { address = fn (address thing) })

cityLens :: Lens' Address String
cityLens = Lens' city (\fn thing -> thing { city = fn (city thing) })

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a = Lens s s a a

set  :: Lens' s a -> a -> s -> s         -- set    value (setter)
set ln value thing = runIdentity $ ln (\_ -> Identity value) thing

view :: Lens' s a -> s -> a              -- lookup value (getter)
view ln th = getConst $ ln (\x -> Const x) th

over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over ln f th = set ln (f (view ln th)) th

# real Lens
import Control.Lens
type Lens' a b = forall f . (Functor f) => (b -> f b) -> (a -> f a)
makeLenses ''Game

# lens laws 
-- Get-Put
>>> forall $ \whole -> set l (view l whole) whole == whole
True
-- Put-Get
>>> forall $ \whole part -> view l (set l part whole) == part
True
-- Put-Put
>>> forall $ \whole part1 part2 -> 
    set l part2 (set l part1) whole = set l part2 whole

# prisms -- что это??
preview :: Prism' s a -> s -> Maybe a
review :: Prism' s a -> a -> s
_Left :: Prism' (Either a b) a
_Just :: Prism' (Maybe a) a

ghci> preview _Left (Left "hi")
Just "hi"
ghci> preview _Left (Right "hi")
Nothing
ghci> review _Left "hi"
Left "hi"
ghci> preview _Just (Just "hi")
Just "hi"
ghci> review _Just "hi"
Just "hi"
ghci> Left "hi" ^? _Left
Just "hi"

12. parallelism
Haskell parallelism advantages:
1. Pure чистый
2. Deterministic статический (определенный?)
3. Flexible гибкий

data Eval a                  -- Eval is monad for parallel computation
instance Monad Eval where
runEval :: Eval a -> a  -- pull the result out of the monad
rpar :: a -> Eval a  -- suggest to parallel, create *spark* 
rseq :: a -> Eval a  -- wait for evaluation of argument (eval it to WHNF)

-- сравнение rpar rseq
-- стратегии
-- Par Monad

13. comonads

class Functor w => Comonad w where
    extract   :: w a -> a
    (<<=)     :: (w a -> b) -> w a -> w b  -- extend
    duplicate :: w a -> w (w a)

-- example 
data Identity a = Identity { runIdentity :: a }
instance Comonad Identity where
    extract   = runIdentity
    duplicate = Identity    


We can emulate some OOP patterns
1. Initial state
2. Builder pattern
3. Iterator pattern (or infinite streams)
4. Command pattern

-- пропуск!!

# Traced Comonad
newtype Traced m a = Traced { runTraced :: m -> a }
instance Monoid m => Comonad (Traced m) where
    extract  (Traced ma) = ma mempty
    extend f (Traced ma) = Traced $ \m -> f (Traced $ \m' -> ma (m <> m'))

type ConfigBuilder = Traced [Option] Config

profile :: ConfigBuilder -> Config
profile builder = runTraced builder ["-prof", "-auto-all"]

goFaster :: ConfigBuilder -> Config
goFaster builder = runTraced builder ["-O2"]

# Store comonad
data Store s a = Store (s -> a) s

class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate = extend id

  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL extract, (duplicate | extend) #-}
#endif
instance Comonad (Store s) where
    extract  (Store f s) = f s
    extend f (Store g s) = Store (f . Store g) s 

type Lens s a = a -> Store s a

# Comonad transformers
class ComonadTrans t where
    lower :: Comonad w => t w a -> w a

15.
applyTwo :: forall b c . (forall a . [a] -> [a]) -> [b] -> [c] -> ([b], [c])
applyTwo f x y = (f x, f y)

ghci> applyTwo id [2,1,3] [True,False]
([2,1,3],[True,False])
ghci> applyTwo reverse [True,False] "patak"
([False,True],"katap")    

# Ranks
Rank 0: Int
Rank 1: forall a . a -> Int
Rank 2: (forall a . a -> Int) -> Int           -- could be enabled by Rank2Types
Rank 3: ((forall a . a -> Int) -> Int) -> Int
The rank of a type describes the depth at which universal quantifiers appear 
in a contravariant position, i.e. to the left of a function arrow.
A function type has rank n + 1 when its argument has rank n.
Int -> Int                                   -- rank 0
forall a . a -> a                            -- rank 1
(forall a . a -> a) -> Int                   -- rank 2
forall a . Int -> a -> a                     -- rank 1
forall a b . a -> b -> a                     -- rank 1 
forall a b . a -> b -> a                     -- rank 1
(a -> a) -> (forall b . b -> b) -> (c -> c)  -- rank 2

-- лееньь

16. Idris
-- Unary multiplication
mult : Nat -> Nat -> Nat
mult Z     y = Z
mult (S k) y = plus y (mult k y)
-- sum
sum : (single : Bool) -> isSingleton single -> Nat
sum True x = x
sum False [] = 0
sum False (x :: xs) = x + sum False xs
-- reverse
reverse : List a -> List a
reverse xs = revAcc [] xs where
  revAcc : List a -> List a -> List a
  revAcc acc [] = acc
  revAcc acc (x :: xs) = revAcc (x :: acc) xs