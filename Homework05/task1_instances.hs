{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
--{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
--import Control.Monad.Identity
import Prelude hiding (Either, Left, Right)

main :: IO()
main = do
	print $ "HW5.1"

--  ====FUNCTOR====
--class Functor f where               -- f :: * -> *
--    fmap :: (a -> b) -> f a -> f b
newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Ord)

data Either a b  =  Left a | Right b
  deriving (Eq, Ord, Read, Show)

data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: Forest a   -- ^ zero or more child trees
    }
type Forest a = [Tree a]
newtype Const a b = Const { getConst :: a }

data Pair a b = Pair a b

instance Functor Identity where
	fmap f (Identity x) = Identity $ f x

instance Functor (Either a) where
	fmap f (Right x) = Right $ f x
	fmap _ (Left x) = Left x

instance Functor Tree where
	fmap f x = Node {rootLabel = f (rootLabel x), subForest = map (fmap f) (subForest x)}

instance Functor (Const a) where
	fmap _ (Const x) = Const x

instance Functor (Pair a) where
	fmap f (Pair x y) = Pair x (f y)

--  ====Applicative===
--class Functor f => Applicative f where
--    -- | Lift a value.
--    pure :: a -> f a
--    -- | Sequential application.
--    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Identity where
	pure = Identity
	(Identity f) <*> x = fmap f x

instance Applicative (Either a) where
	pure = Right
	(Left err) <*> _ = Left err
	(Right f) <*> x = fmap f x

instance Applicative Tree where
	pure x = Node {rootLabel = x, subForest = []}
	f <*> x = fmap (rootLabel f) x

instance Monoid a => Applicative (Const a) where
	pure _ = Const mempty
	(Const f) <*> (Const x) = Const $ mappend f x

instance Monoid a => Applicative (Pair a) where
	pure = Pair mempty
	(Pair _ f) <*> x = fmap f x

--  ====FOLDABLE===
--class Foldable t where
--    foldMap :: Monoid m => (a -> m) -> t a -> m
--    foldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable Identity where
	foldMap f (Identity x) = f x

instance Foldable (Either a) where
	foldMap f (Right x) = f x
	foldMap _ (Left _) = mempty

instance Foldable Tree where
	foldMap f tree = f (rootLabel tree)

instance Foldable (Const a) where
	foldr _ x _ = x

instance Foldable (Pair a) where
	foldMap f (Pair _ y) = f y

-- =====TRAVERSABLE====
--class (Functor t, Foldable t) => Traversable t where
--    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable Identity where
	traverse f (Identity x) = Identity <$> f x

instance Traversable (Either a) where
	traverse f (Right x) = Right <$> f x
	traverse _ (Left err) = pure (Left err)

instance Traversable Tree where
	traverse f (Node x forest) = (Node <$> f x) <*> traverse (traverse f) forest

instance Traversable (Const a) where
	traverse _ (Const x) = pure (Const x)

instance Traversable (Pair a) where
	traverse f (Pair x y) = Pair x <$> f y
