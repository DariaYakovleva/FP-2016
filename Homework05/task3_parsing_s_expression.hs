{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
--{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
import AParser
import Data.Char
import Control.Applicative

main :: IO()
main = do
	print $ "HW5.3"
	print $ runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" -- Just ("ABC","dEfgH")	
	print $ runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" -- Just ("ABC","dEfgH")
	print $ runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" -- Just ("","abcdeFGh")
	print $ runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" -- Nothing
	print $ "!!spaces"
	print $ runParser spaces "    abcdeFGh" -- Nothing
	print $ "!!ident"
	print $ runParser ident "foobar baz" --Just ("foobar"," baz")
	print $ runParser ident "foo33fA" --Just ("foo33fA","")
	print $ runParser ident "2bad" --Nothing
	print $ runParser ident "" --Nothing
	print $ "!!SExpr"
	print $ runParser parseSExpr "5"
	print $ runParser parseSExpr "foo3"
	print $ runParser parseSExpr "(bar (foo) 3 5 874)"
	print $ runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
	print $ runParser parseSExpr "(   lots  of   (  spaces   in  )  this ( one ) )"


oneOrMore :: Parser a -> Parser [a]
oneOrMore p = Parser $ \s -> case runParser p s of
								Nothing -> Nothing
								Just (x, y) -> case runParser (zeroOrMore p) y of									
									Just (x', y') -> Just (x:x', y')
									Nothing -> Nothing

zeroOrMore  :: Parser a -> Parser [a]
zeroOrMore p = Parser $ \s -> let res = runParser (oneOrMore p) s in case res of 
															Nothing -> Just ([], s)
															Just _ -> res

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (++) <$> (oneOrMore $ satisfy isAlpha) <*> (zeroOrMore $ satisfy isAlphaNum)

type Ident = String

data Atom = N Integer | I Ident
	deriving Show
data SExpr = A Atom | Comb [SExpr]
	deriving Show

--5
--foo3
--(bar (foo) 3 5 874)
--(((lambda x (lambda y (plus x y))) 3) 5)
--(   lots  of   (  spaces   in  )  this ( one ) 
parseSExpr :: Parser SExpr
parseSExpr = spaces *> parseAtom <|> parseComb <* spaces

parseComb :: Parser SExpr
parseComb = Comb <$> ((satisfy (== '(')) *> (zeroOrMore parseSExpr) <* (satisfy (== ')')))

parseAtom :: Parser SExpr
parseAtom =  spaces *> (A <$> ((N <$> posInt) <|> (I <$> ident))) <* spaces