-- TASK 1. Implement selN function using fstN as example.
--ghci> $(selN 4 1) ("hello", 1, [4,3], 2)
--"hello"
--ghci> $(selN 4 3) ("hello", 1, [4,3], 2)
--[4,3]

{-# LANGUAGE TemplateHaskell #-}

module SelN where
import Language.Haskell.TH

selN :: Int -> Int -> Q Exp
selN len pos  = do
   x <- newName "x"
   return $ LamE [TupP $ replicate (pos - 1) WildP ++ [VarP x] ++ replicate (len - pos) WildP] (VarE x)