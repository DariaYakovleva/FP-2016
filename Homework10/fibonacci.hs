-- {-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances, RankNTypes #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Fibonacci where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Num

--fib :: Int -> Q Exp
--fib 0 = return $ LitE (IntegerL 1)
--fib 1 = return $ LitE (IntegerL 1)
--fib n = do
--	f1 <- fib (n - 1)
--	f2 <- fib (n - 2)
--	return (InfixE (Just f1) (VarE GHC.Num.+) (Just f2))


fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

getFib :: Int -> Q Exp
getFib n = runQ [| fibs !! n |]
