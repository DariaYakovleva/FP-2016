--TASK
--Define data type for arithmetic expressions with: Constants, Variables, Sum, Multiplication and Local bindings.
--Implement evaluation function for your data type. Use Map for variables. 
--Use Reader monad. Think of possibility of not having variable in Map.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
--{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

import Control.Monad.Reader
import Data.Map

main :: IO()
main = do
	print $ "HW6.2"
	print $ expr
	print $ runReader (evaluate expr) (insert "x" 1 empty)

--data Reader r a = Reader {  runReader :: r -> a }

type Variable = String
type Constant = Integer
data Exp = Lit Constant
			| Var Variable
			| Add Exp Exp
			| Mul Exp Exp
			| Assign Variable Constant Exp
	deriving (Show)

-- x = 1 is 7
expr :: Exp
expr = Var "x" `Add` (Lit 3 `Mul` ("x" `Assign` 2 $ Var "x"))


value :: Map Variable Constant -> Variable -> Constant
value mp var = mp ! var

evaluate :: Exp -> Reader (Map Variable Constant) Integer
evaluate (Lit x) = return x
evaluate (Var x) = reader $ \mp -> value mp x
evaluate (Add x y) = liftM2 (+) (evaluate x) (evaluate y)
evaluate (Mul x y) = liftM2 (*) (evaluate x) (evaluate y)
evaluate (Assign var val ex) = reader $ \mp -> (runReader (evaluate ex) (insert var val mp))