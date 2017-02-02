--Implement parsing of arithmetic expressions like this one:
---1 + (2 * 3 *( 4 - 5))^4
--You should also implement this as console utility with different modes & cmd arguments. Examples of usage:

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import qualified Options.Applicative as AP
import Data.Semigroup ((<>))
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Control.Monad.Reader
import Data.Maybe
import Data.Map
import Data.List.Split

-- $ expr-parsing print-ast --expr "1+x-3"
-- Add (Lit 1) (Sub (Var "x") (Lit 3))
-- $ expr-parsing eval --expr "1+x-3"
-- Evaluation error: variable `x` is not defined
-- $ expr-parsing eval --expr "1+x-3" --var "(x,10)"
-- 8


data Args = Args
  { action :: Command
  , expr  :: String
  , var  :: [String]
  } 

data Command
  = PrintAst
  | Eval

exprArgs :: AP.Parser Args
exprArgs = Args
	AP.<$> AP.subparser (
		   AP.command "print-ast" (AP.info (AP.helper <*> pure PrintAst) AP.idm)
 		<> AP.command "eval"      (AP.info (AP.helper <*> pure Eval) AP.idm) ) 
     AP.<*> AP.strOption
         ( AP.long "expr"
        <> AP.metavar "EXPRESSION"
        <> AP.help "Expression for parsing" )     
     AP.<*> many (AP.strOption
         ( AP.long "var"
        <> AP.help "Variables for expression"        
        <> AP.metavar "(variable, value)" ) )


readTup :: [String] -> [(String, Integer)]
readTup [] = []
readTup (x:xs) = let [_, xx, y, _] = splitOneOf "()," x in ([(xx, read y)] ++ readTup xs)

greet :: Args -> IO ()
greet (Args act exprr vars) = case act of
	Eval -> do 
		let exx = fromMaybe (Var "Parsing failed") (parseMaybe aExpr exprr)
		print $ runReader (evaluate exx) (fromList (readTup vars))
	PrintAst -> parseTest aExpr exprr

main :: IO ()
main = do
	putStrLn "HW7.2"	
	AP.execParser opts >>= greet
	  where
	    opts = AP.info (AP.helper <*> exprArgs)
	      ( AP.fullDesc
	     <> AP.progDesc "Print a expression"
	     <> AP.header "expr - expression for parsing" )         

type Variable = String
type Constant = Integer

data AExpr = Var Variable
           | Lit Constant
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show)

data ABinOp = Add
            | Sub
            | Mul
            | Div
              deriving (Show)

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.integer

identifier :: Parser String
identifier = (lexeme . try) ((:) <$> letterChar <*> many alphaNumChar)  

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (symbol "-" *> pure Neg) ]
  , [ InfixL (symbol "*" *> pure (ABinary Mul))
    , InfixL (symbol "/" *> pure (ABinary Div)) ]
  , [ InfixL (symbol "+" *> pure (ABinary Add))
    , InfixL (symbol "-" *> pure (ABinary Sub)) ]
  ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

aTerm :: Parser AExpr
aTerm = parens aExpr
     <|> Var      <$> identifier
     <|> Lit <$> integer

value :: Map Variable Constant -> Variable -> Constant
value mp varr = mp ! varr

evaluate :: AExpr -> Reader (Map Variable Constant) Integer
evaluate (Lit x) = return x
evaluate (Var x) = reader $ \mp -> value mp x
evaluate (Neg x) =  fmap (0 -) (evaluate x)
evaluate (ABinary Add x y) = liftM2 (+) (evaluate x) (evaluate y)
evaluate (ABinary Sub x y) = liftM2 (-) (evaluate x) (evaluate y)
evaluate (ABinary Mul x y) = liftM2 (*) (evaluate x) (evaluate y)
evaluate (ABinary Div x y) = liftM2 div (evaluate x) (evaluate y)