-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.MetaSearch.Parser(MonadParser,Parser(..),Consumed(..),Reply(..),parse,parseResult,fmap,return,(>>=),mzero,mplus,
	required,satisfy,tryParser,untilParser,matchParser,many,many1,char,single,skipSpace,skipBlanks,match,
	untilChar,whileChar,string,stringNcs,untilEnd,mapContains,trimText,getWords,isBlankText,isBlank,
	consume,matchEnd,untilParserOrEnd,splitWords) where

-- note: space is considered both horizontal and vertical white space,
-- blanks are just horizontal (ie not newline or return).

import Char
import Monad

{-# INLINE isBlank #-}
isBlank :: Char -> Bool
isBlank c = (c==' ' || c=='\t' || c=='\r')

newtype Parser a = Parser (String -> Consumed a)
data Consumed a = Consumed (Reply a) | Empty (Reply a)
data Reply a = Ok a String | Error

{-# INLINE parse #-}
parse :: Parser a -> String -> Consumed a
parse (Parser p) = p

{-# INLINE parseResult #-}
parseResult :: Parser a -> String -> Reply a
parseResult (Parser p) cs = case p cs of
	Consumed result -> result 
	Empty result -> result

instance Functor Parser where
	{-# INLINE fmap #-}
	fmap f (Parser p) = Parser (\cs -> case p cs of
		Consumed reply -> Consumed $ case reply of
			Ok x cs' -> Ok (f x) cs'
			otherwise -> Error
		Empty reply -> Empty $ case reply of
			Ok x cs' -> Ok (f x) cs'
			otherwise -> Error)

instance Monad Parser where
	{-# INLINE return #-}
	return a	= Parser (\cs -> Empty (Ok a cs))
	{-# INLINE (>>=) #-}
	(Parser p) >>= f = Parser (\cs -> case p cs of
		Empty reply1 -> case reply1 of
			Ok x cs' -> parse (f x) cs'
			Error -> Empty Error
		Consumed reply -> Consumed $ case (reply) of
			Ok x rest -> case parse (f x) rest of
				Consumed reply' -> reply'
				Empty reply' -> reply'
			Error -> Error)

instance MonadPlus Parser where
	{-# INLINE mzero #-}
	mzero = Parser (\cs -> Empty Error)
	{-# INLINE mplus #-}
	mplus (Parser p) (Parser q) = Parser (\cs -> case p cs of
		Empty Error -> q cs
		other -> other)

class (MonadPlus m) => MonadParser m where
	matchParser :: m a -> m b -> m a
	untilParser :: m a -> m b -> m [b]
	untilParserOrEnd :: m a -> m b -> m [b]
	required :: m a -> m a
	tryParser :: m a -> m a
	skipSpace :: m a -> m a
	skipBlanks :: m a -> m a
	many1 :: m a -> m [a]
	many  :: m a -> m [a]
	single :: m a -> m [a]
	consume :: m a -> m ()
	
instance MonadParser Parser where
	{-# INLINE tryParser #-}
	tryParser (Parser p) = Parser (\cs -> case p cs of
		Consumed Error -> Empty Error
		other -> other)

	{-# INLINE required #-}
	required (Parser p) = Parser (\cs -> case p cs of
		Empty _ -> Empty Error
		consumed -> consumed)

	{-# INLINE skipSpace #-}
	skipSpace p = do
		a <- p
		(consume . match) isSpace
		return a

	{-# INLINE skipBlanks #-}
	skipBlanks p = do
		a <- p
		(consume . match) isBlank
		return a

	consume p = Parser (\cs -> case parse p cs of
		Consumed reply -> Consumed $ case reply of
			Ok _ cs' -> case parse (many p) cs' of
				Consumed reply' -> case reply' of
					Ok _ cs'' -> Ok () cs''
					Error -> Error
				Empty reply' -> case reply' of
					Ok _ cs'' -> Ok () cs''
					Error -> Error
			Error -> Error
		Empty reply -> Empty $ case reply of
			Ok _ cs' -> Ok () cs'
			Error -> Ok () cs)

	many p = Parser (\cs -> case parse p cs of
		Consumed reply -> Consumed $ case reply of
			Ok a cs' -> case parse (many p) cs' of
				Consumed reply' -> case reply' of
					Ok b cs'' -> Ok (a:b) cs''
					Error -> Error
				Empty reply' -> case reply' of
					Ok b cs'' -> Ok (a:b) cs''
					Error -> Error
			Error -> Error
		Empty reply -> Empty $ case reply of
			Ok a cs' -> Ok [a] cs'
			Error -> Ok [] cs)

	many1 p = Parser (\cs -> case parse p cs of
		Consumed reply -> Consumed $ case reply of
			Ok a cs' -> case parse (many p) cs' of
				Consumed reply' -> case reply' of	
					Ok b cs'' -> Ok (a:b) cs''
					Error -> Error
				Empty reply' -> case reply' of
					Ok b cs'' -> Ok (a:b) cs''
					Error -> Error
			Error -> Error
		Empty reply -> Empty $ case reply of
			Ok a cs' -> Ok [a] cs'
			Error -> Error)

	{-# INLINE single #-}
	single p = Parser (\cs -> case parse p cs of
		Consumed reply -> Consumed $ case reply of
			Ok a cs' -> Ok [a] cs'
			otherwise -> Error
		Empty reply -> Empty $ case reply of
			Ok a cs' -> Ok [a] cs'
			otherwise -> Error)

	matchParser p q = Parser (\cs -> case parse (tryParser p) cs of
		Empty _ -> case parse q cs of
			Empty _ -> Empty Error
			Consumed result -> Consumed $ case result of
				Ok a cs' -> case parse (matchParser p q) cs' of
					Consumed result' -> result' 
					Empty result' -> result'
				Error -> Error
		consumed -> consumed)

	untilParserOrEnd p q = Parser (\cs -> case parse p cs of
		Consumed result -> case result of 
			Ok _ cs' -> Empty (Ok [] cs')
			otherwise -> termFailed cs   
		Empty result -> case result of
			Ok _ cs' -> Empty (Ok [] cs')
			otherwise -> termFailed cs) where

				termFailed cs = case parse q cs of
					Consumed result -> Consumed $ case result of
						Ok x cs' -> case parse (untilParserOrEnd p q) cs' of
							Consumed result' -> case result' of
								Ok y cs'' -> Ok (x:y) cs''
								otherwise -> Error
							Empty _ -> Ok [x] cs'
 						otherwise -> Error
					Empty result -> Empty $ case result of
						Ok x cs' -> Ok [x] cs'
						otherwise -> Error
	
	untilParser p q = Parser (\cs -> case parse p cs of
		Consumed result -> case result of 
			Ok _ cs' -> Empty (Ok [] cs')
			otherwise -> termFailed cs   
		Empty result -> case result of
			Ok _ cs' -> Empty (Ok [] cs')
			otherwise -> termFailed cs) where

				termFailed cs = case parse q cs of
					Consumed result -> Consumed $ case result of
						Ok x cs' -> case parse (untilParser p q) cs' of
							Consumed result' -> case result' of
								Ok y cs'' -> Ok (x:y) cs''
								otherwise -> Error
							Empty result' -> case result' of
								Ok y cs'' -> Ok (x:y) cs''
								otherwise -> Error
 						otherwise -> Error
					Empty result -> Empty $ case result of
						Ok x cs' -> Ok [x] cs'
						otherwise -> Error
	
{-# INLINE matchEnd #-}
matchEnd :: Parser ()
matchEnd = Parser (\cs -> case cs of
	[] -> Empty (Ok () cs)
	otherwise -> Empty Error)

{-# INLINE mkParser #-}
mkParser :: (String -> String) -> Parser ()
mkParser f = Parser (\cs -> Empty (Ok () (f cs)))

{-# INLINE char #-}
char :: Parser Char
char = Parser (\cs -> case cs of
	[] -> Empty Error
	(c:cs') -> Consumed (Ok c cs'))

{-# INLINE satisfy #-}
satisfy :: (Char -> Bool) -> Parser Char
satisfy isType = Parser (\cs -> case cs of
	[] -> Empty Error
	(c:cs')	| isType c -> Consumed (Ok c cs')
				| otherwise -> Empty Error)

{-# INLINE match #-}
match :: (Char -> Bool) -> Parser ()
match isType = Parser (\cs -> case cs of
	[] -> Empty Error
	(c:cs') | isType c -> Consumed (Ok () cs')
			  | otherwise -> Empty Error)

whileChar :: (Char ->  Bool) -> Parser String
whileChar isType = Parser (\cs -> case cs of
	(c:cs') -> if isType c
		then Consumed $ case parse (whileChar isType) cs' of
			Consumed reply -> case reply of
				Ok d cs'' -> Ok (c:d) cs''
				Error -> Error
			Empty reply -> case reply of
				Ok d cs'' -> Ok (c:d) cs''
				Error -> Error
		else Empty (Ok [] cs)
	otherwise -> Empty (Ok [] cs))

untilChar :: (Char -> Bool) -> Parser String
untilChar isType = Parser (\cs -> case cs of
	(c:cs') -> if isType c
		then Empty (Ok [] cs)
		else Consumed $ case parse (untilChar isType) cs' of
			Consumed reply -> case reply of
				Ok d cs'' -> Ok (c:d) cs''
				Error -> Error
			Empty reply -> case reply of
				Ok d cs'' -> Ok (c:d) cs''
				Error -> Error
	otherwise -> Empty (Ok [] cs))

untilEnd :: Parser String
untilEnd = Parser (\cs -> case cs of
	(c:cs') -> Consumed $ case parse untilEnd cs' of
		Consumed reply -> case reply of
			Ok d cs'' -> Ok (c:d) cs''
			Error -> Error
		Empty reply -> case reply of
			Ok d cs'' -> Ok (c:d) cs''
			Error -> Error
	otherwise -> Empty (Ok [] cs))

-- compare string with input
string :: String -> Parser ()
string s = Parser (\cs -> strcons s cs) where
	strcons :: String -> String -> Consumed ()
	strcons [] y = Empty (Ok () y)
	strcons x [] = Empty Error
	strcons (x0:x') (y0:y')
		| x0 == y0 = Consumed $ strreply x' y'
		| otherwise = Empty Error where
			strreply :: String -> String -> Reply ()
			strreply [] y = Ok () y
			strreply x [] = Error
			strreply (x0:x') (y0:y')
				| x0 == y0 = strreply x' y'
				| otherwise = Error

-- compare string with input, not case sensitive
stringNcs :: String -> Parser ()
stringNcs s = Parser (\cs -> ncscons s cs) where
	ncscons :: String -> String -> Consumed ()
	ncscons [] y = Empty (Ok () y)
	ncscons x [] = Empty Error
	ncscons (x0:x') (y0:y')
		| (toLower x0) == (toLower y0) = Consumed $ ncsreply x' y'
		| otherwise = Empty Error where
			ncsreply :: String -> String -> Reply ()
			ncsreply [] y = Ok () y
			ncsreply x [] = Error
			ncsreply (x0:x') (y0:y')
				| (toLower x0) == (toLower y0) = ncsreply x' y'
				| otherwise = Error

-- usful parsers for string manipulation ------------------

-- is string just spaces (and CR/LF)
isBlankText :: String -> Bool
isBlankText [] = True
isBlankText s@(c:cs) = case parse (stringNcs "&nbsp;") s of
	Consumed (Ok _ s') -> isBlankText s'
	otherwise -> if isSpace c then isBlankText cs else False

-- trim string and replace multiple spaces
trimText :: String -> String
trimText [] = []
trimText s@(c:cs) = if isBlankText s then [] else case parse (stringNcs "&nbsp;" ) s of
	Consumed (Ok _ s') -> trimText s'
	otherwise -> if isSpace c then trimText cs else c:trimAlt cs where
		trimAlt :: String -> String
		trimAlt [] = []
		trimAlt s@(c:cs) = if isBlankText s then [] else case parse (stringNcs "&nbsp;") s of
			Consumed (Ok _ s') -> ' ':trimText s'
			otherwise -> if isSpace c then ' ':trimText cs else c:trimAlt cs

trimParser :: Parser ()
trimParser = mkParser trimText

-- tests if string contains alpha chars
hasAlphas :: String -> Bool
hasAlphas [] = False
hasAlphas (s0:s') = if isAlpha s0
	then True
	else hasAlphas s'

-- remove all non alpha chars and compact spaces
_getWords' :: String -> String
_getWords' [] = []
_getWords' (s0:s') = if isAlpha s0
	then s0:_getWords' s'
	else if hasAlphas s'
		then ' ':_getWords s'
		else []

_getWords :: String -> String
_getWords [] = []
_getWords (s0:s') = if isAlpha s0
	then s0:_getWords' s'
	else if hasAlphas s'
		then _getWords s'
		else []

getWords :: Parser ()
getWords = mkParser _getWords

-- return rhs of pair if all strings on lhs match 
_mapContains :: [String] -> Parser ()
_mapContains [] = Parser (\cs -> Empty (Ok () cs))
_mapContains (t0:t') = Parser (\cs -> case parse (matchParser (stringNcs t0) char) cs of
	Consumed (Ok _ _) -> Consumed $ case parse (_mapContains t') cs of
		Consumed result -> result
		Empty result -> result
	otherwise -> Empty Error)

mapContains :: [([String],String)] -> Parser String
mapContains [] = Parser (\_ -> Empty Error)
mapContains ((s,t):st') = Parser (\cs -> case parse (_mapContains s) (_getWords cs) of
	Consumed (Ok () cs') -> Consumed (Ok t cs')
	_ -> parse (mapContains st') cs)

splitWords :: Parser [String]
splitWords = do
	whileChar isSpace
	a <- (required . untilChar) isSpace
	as <- splitWords `mplus` return []
	return (a:as)
