{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Tree2
       ( Tree.Tree (..)
       , Tree.directoryPrint
       , Tree.verticalPrint
       ) where

import qualified MyTree as Tree

main = do
	print $ "d"
	print $ mconcat [t1, t2, t3]
	print $ t4
	--print $ stoList t4
	print $ mapTree (\x -> (x, x)) t4


t1 = Tree.Node 3 Tree.Leaf Tree.Leaf
t2 = Tree.Node 4 Tree.Leaf Tree.Leaf
t3 = Tree.Node 2 Tree.Leaf Tree.Leaf
t4 = foldr mappend Tree.Leaf [t1, t2, t3]

--  mempty  :: a
--  mappend :: a -> a -> a
--  mconcat :: [a] -> a
instance Ord a => Monoid (Tree.Tree a) where
	mempty = Tree.Leaf
	mappend Tree.Leaf t = t
	mappend t Tree.Leaf = t
	mappend t (Tree.Node val l r) = Tree.insert (mappend (mappend t l) r) val
	mconcat tlist = foldl mappend Tree.Leaf tlist

--  foldr :: (a -> b -> b) -> b -> t a -> b
instance Foldable Tree.Tree where
	foldr f neutral Tree.Leaf = neutral
	foldr f neutral (Tree.Node val Tree.Leaf Tree.Leaf) = f val neutral
	foldr f neutral (Tree.Node val l r) = foldr f (f val (foldr f neutral r)) l

mapTree :: (a -> b) -> Tree.Tree a -> Tree.Tree b
mapTree _ Tree.Leaf           = Tree.Leaf
mapTree f (Tree.Node val l r) = Tree.Node (f val) (mapTree f l) (mapTree f r)

class Map t k v where
    memptyMap :: t (k, v)
    mtoList   :: t (k, v) -> [(k, v)]
    mfind     :: t (k, v) -> (k, v) -> t (k, v)
    minsert   :: t (k, v) -> (k, v) -> t (k, v)
    mdelete   :: t (k, v) -> (k, v) -> t (k, v)
    mnext     :: t (k, v) -> (k, v) -> Maybe (k, v)
    mfromList :: [(k, v)] -> t (k, v)

instance (Ord k, Ord v, Foldable Tree.Tree, Monoid (Tree.Tree k)) => Map Tree.Tree k v where
	memptyMap = Tree.Leaf
	mtoList = foldr (:) []
	mfind  = Tree.find
	minsert  = Tree.insert
	mdelete  = Tree.delete
	mnext    = Tree.next
	mfromList = foldr (flip Tree.insert) Tree.Leaf

class Set t a where
    semptySet :: t (a, a)
    stoList   :: t (a, a) -> [a]
    sfind     :: t (a, a) -> a -> t (a, a)
    sinsert   :: t (a, a) -> a -> t (a, a)
    sdelete   :: t (a, a) -> a -> t (a, a)
    snext     :: t (a, a) -> a -> Maybe a
    sfromList :: [a] -> t (a, a)

instance (Map Tree.Tree k k) => Set Tree.Tree k where
	semptySet = memptyMap
	stoList tree = map fst (mtoList tree)
	sfind tree x = mfind tree (x, x)
	sinsert tree x = minsert tree (x, x)
	sdelete tree x = mdelete tree (x, x)
	snext tree x = case (mnext tree (x, x)) of
		Nothing -> Nothing
		Just (x, y) -> Just x
	sfromList lst = mfromList $ map (\x -> (x, x)) lst
