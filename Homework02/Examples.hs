--import qualified Data.Char as C hiding (chr)
main = do
    print $ "Lection 2"

type MyType = Int -> Int

data Equipment = Health | Attack | Defense

data Player = Player String
getName :: Player -> String
getName (Player name) = name

data Vector a = Vector2D a a | Vector3D a a a --need smth
vectorSum :: Vector Double -> Double
vectorSum (Vector2D x y) = x + y
vectorSum (Vector3D x y z) = x + y + z

data Maybe a = Just a | Nothing
data Either a b = Left a | Right b -- left = error, right = value

data List a = Nil | Cons a (List a)

--records
data User = User 
	{ uid	:: Int
	, login :: String
	, password :: String
	}
ivan :: User 
ivan = User {uid = 1, login = "ivan", password = "123"}

isIvan :: User -> Bool
isIvan User {login = username} = (username == "ivan")

data Person 
	= User2 {id :: String}
	| Player2 {id :: String}

--classes

class Printable p where
	printMe :: p -> String

data Foo = Foo
instance Printable Foo where
	printMe Foo = "Foo"

helloP :: Printable p => p -> String
helloP p = printMe p

class I a where
    measure :: a -> Int -> Double

class J a where
    getParameter :: a -> Int

doMeasure :: (I a, J a) => a -> Double
doMeasure obj = measure obj $ getParameter obj

cmpSum :: (Num a, Ord a) => a -> a -> a
cmpSum x y = if x < y then x + y else x * y

foo :: (Ord a, Read a, Show b) => String -> a -> b -> b -> String
foo = undefined -- too difficult to implement

data TrafficLight = Red | Yellow | Green | Blue
    deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Box a  = Box a              deriving Show

--ordering
data Ordering = LT | EQ | GT
-- simplified version of Ord class
class Eq a => Ord a where
   compare              :: a -> a -> Ordering
   (<), (<=), (>=), (>) :: a -> a -> Bool

   compare x y
        | x == y    =  EQ
        | x <= y    =  LT
        | otherwise =  GT

   x <= y           =  compare x y /= GT
   x <  y           =  compare x y == LT
   x >= y           =  compare x y /= LT
   x >  y           =  compare x y == GT

data Point a = Point a a deriving (Eq, Ord)
instance                  Ord (Point a)
instance  Eq a         => Ord (Point a)
instance        Ord a  => Ord (Point a)
instance (Eq a, Ord a) => Ord (Point a)
