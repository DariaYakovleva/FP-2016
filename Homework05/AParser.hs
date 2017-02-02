{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
--{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}
module AParser 
	( Parser (..)
	, satisfy	
	, isUpper
	, posInt
	, abParser
	, abParser_
	, intPair
	, intOrUppercase

	) where

import Control.Monad
import Data.List()
import Data.Char
import Control.Applicative

main :: IO()
main = do
	print $ "HW5.2"
	print $ runParser abParser "abcde"
	print $ runParser abParser "aebcd"
	print $ runParser abParser_ "abcde"
	print $ runParser abParser_ "aebcd"
	print $ runParser posInt "1 2"
	print $ runParser intPair "12 34"
	print $ runParser intOrUppercase "342abcd"
	print $ runParser intOrUppercase "XYZ"
	print $ runParser intOrUppercase "foo"


newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f 
  where
	f [] = Nothing    -- fail on the empty input
	f (x:xs)          -- check if x satisfies the predicate	
		| p x       = Just (x, xs)
		| otherwise = Nothing  -- otherwise, fa

posInt :: Parser Integer
posInt = Parser f
  where
	f xs
		| null ns   = Nothing
		| otherwise = Just (read ns, rest)
		where (ns, rest) = span isDigit xs

first :: (a -> b) -> Maybe (a, c) -> Maybe (b, c)
first f (Just (x, y)) = Just (f x, y)
first _ Nothing = Nothing

--    fmap :: (a -> b) -> f a -> f b
instance Functor Parser where
	fmap f (Parser p) = Parser $ \s -> first f (p s)

--    pure :: a -> f a
--    (<*>) :: f (a -> b) -> f a -> f b
instance Applicative Parser where
	pure x = Parser $ \s -> Just (x, s)
	(Parser f) <*> p = Parser $ \s ->
									case f s of 
										Just (f', s') -> runParser (fmap f' p) s'
										Nothing -> Nothing


-- Just ((’a’,’b’),"cdef")
abParser :: Parser (Char, Char)
abParser = ((,) <$> satisfy (== 'a')) <*> satisfy (== 'b')


-- Just ((),"cdef")
abParser_ :: Parser ()
abParser_ = void abParser


--runParser intPair "12 34" = Just ([12,34],"")
intPair :: Parser [Integer]
intPair = (\x y -> [x, y]) <$> (posInt <* satisfy (== ' ')) <*> posInt

instance Alternative Parser where
	empty = Parser $ const Nothing
	(Parser p1) <|> (Parser p2) = Parser $ \s ->
							  case p1 s of
							  	Nothing -> p2 s
							  	Just x -> Just x

--runParser intOrUppercase "342abcd" = Just ((), "abcd")
--runParser intOrUppercase "XYZ" =Just ((), "YZ")
--runParser intOrUppercase "foo" = Nothing
intOrUppercase :: Parser ()
intOrUppercase =  void (satisfy isUpper) <|> void posInt