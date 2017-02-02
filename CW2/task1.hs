{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
--{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

-- TASK
-- имеется набор функций Monad m => a -> m a, которые необходимо применить последовательно к заданному элементу. 
-- Напишите функцию, реализующую требуемую логику.

main :: IO()
main = do
	print $ "CW2.1"
	print $ funs 3 [f, g, h] -- = (3 * 4 + 3) * 2
	print $ funs 0 [f, g, h] -- = Nothing

f :: Int -> Maybe Int
f 0 = Nothing
f x = Just (x * 4)

g :: Int -> Maybe Int
g 0 = Nothing
g x = Just (x + 3)

h :: Int -> Maybe Int
h 0 = Nothing
h x = Just (x * 2)

apply :: Monad m => [(a -> m a)] -> a -> m a
apply functions e = foldr (=<<) (return e) functions 

funs :: Monad m => a -> [(a -> m a)] -> m a
funs x ff = funs2 (return x) ff

funs2 :: Monad m => m a -> [(a -> m a)] -> m a
funs2 x [] = x
funs2 x (ff:fs) = funs2 (x >>= ff) fs