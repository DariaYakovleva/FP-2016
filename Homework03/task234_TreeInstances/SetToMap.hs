{-# OPTIONS_GHC -fno-warn-tabs #-}
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
	print $ stoList t4


t1 = Tree.Node 3 Tree.Leaf Tree.Leaf
t2 = Tree.Node 4 Tree.Leaf Tree.Leaf
t3 = Tree.Node 2 Tree.Leaf Tree.Leaf
t4 = foldr mappend Tree.Leaf [t1, t2, t3]

instance Ord a => Monoid (Tree.Tree a) where
	mempty = Tree.Leaf
	mappend Tree.Leaf t = t
	mappend t Tree.Leaf = t
	mappend t (Tree.Node val l r) = Tree.insert (mappend (mappend t l) r) val
	mconcat tlist = foldl mappend Tree.Leaf tlist

instance Foldable Tree.Tree where
	foldr f neutral Tree.Leaf = neutral
	foldr f neutral (Tree.Node val Tree.Leaf Tree.Leaf) = f val neutral
	foldr f neutral (Tree.Node val l r) = foldr f (f val (foldr f neutral r)) l


class Set t a where
    semptySet :: t a
    stoList   :: t a -> [a]
    sfind     :: t a -> a -> t a
    sinsert   :: t a -> a -> t a
    sdelete   :: t a -> a -> t a
    snext     :: t a -> a -> Maybe a
    sfromList :: [a] -> t a

instance (Ord a, Foldable Tree.Tree, Monoid (Tree.Tree a)) => Set Tree.Tree a where
	semptySet = Tree.Leaf
	stoList = foldr (:) []
	sfind  = Tree.find
	sinsert  = Tree.insert
	sdelete  = Tree.delete
	snext    = Tree.next
	sfromList = foldr (flip Tree.insert) Tree.Leaf

class Map t k v where
    memptyMap :: t (k, v)
    mtoList   :: t (k, v) -> [(k, v)]
    mfind     :: t (k, v) -> (k, v) -> t (k, v)
    minsert   :: t (k, v) -> (k, v) -> t (k, v)
    mdelete   :: t (k, v) -> (k, v) -> t (k, v)
    mnext     :: t (k, v) -> (k, v) -> Maybe (k, v)
    mfromList :: [(k, v)] -> t (k, v)

instance (Set Tree.Tree (k, v)) => Map Tree.Tree k v where
	memptyMap = Tree.Leaf
	mtoList = stoList
	mfind = sfind
	minsert  = sinsert
	mdelete  = sdelete
	mnext    = snext
	mfromList = sfromList


