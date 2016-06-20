{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.Parser.Parser(Parser,when,unless,guard,(|>),opt,many,many1,sepBy,
	parse,alpha,digit,lower,upper,other,lexical,satisfy,optional,literal,untilP,untilParser,matchP) where

import Char
import Control.Monad hiding (guard,when,unless)
import Lib.Monad.MonadParser
import Lib.Monad.ParserT
import Lib.Arrow.Runnable

------------------------------------------------------------------------------

when,unless :: Monad m => Bool -> m () -> m ()
when b m = if b then m else return ()
unless b m = if b then return () else m

guard :: MonadPlus m => Bool -> m ()
guard b = if b then return () else mzero

(|>) :: MonadPlus m => m a -> (a -> Bool) -> m a
m |> p = do
	a <- m
	if p a then return a else mzero

opt :: MonadPlus m => m a -> a -> m a
opt m a = m `mplus` return a

literal :: (MonadPlus m,Eq a) => m a -> [a] -> m () 
literal _ [] = return ()
literal m (c0:cs) = do
	i0 <- m
	if i0==c0 then literal m cs else mzero

optional :: MonadPlus m => m a -> m (Maybe a)
optional m = ((do a <- m;return (Just a)) `mplus` (return Nothing))

untilP :: MonadPlus m => m a -> m b -> m [b]
untilP m n = do
		m
		return []
	`mplus` do
		a <- n
		x <- untilP m n
		return (a:x)

untilParser :: MonadPlus m => m a -> m b -> m (a,[b])
untilParser m n = do
		a <- m
		return (a,[])
	`mplus` do
		a <- n
		(x,y) <- untilParser m n
		return (x,a:y)

matchP :: MonadPlus m => m a -> m b -> m a
matchP m n = m `mplus` do
	n
	matchP m n

{-# INLINE many #-}
many :: MonadPlus m => m a -> m [a]
many m = many1 m `mplus` return []

many1 :: MonadPlus m => m a -> m [a]
many1 m = do
	a <- m
	x <- many1 m `mplus` return []
	return (a:x)

sepBy :: MonadPlus m => m a -> m b -> m [a]
sepBy m s = do
	a <- m 
	x <- many (do s;m)
	return (a:x)

{-# INLINE parse #-}
parse :: (MonadPlus m,Runnable (ParserT ([tok],a) tok m a) ([tok] -> m ([tok],a))) =>
	ParserT ([tok],a) tok m a -> [tok] -> m ([tok],a)
parse p s = run p s

------------------------------------------------------------------------------

{-# INLINE lower #-}
{-# INLINE upper #-}
{-# INLINE alpha #-}
{-# INLINE space #-}
{-# INLINE digit #-}
{-# INLINE other #-}
lower,upper,alpha,space,digit,other :: MonadParser Char m => m Char
lower = item |> isLower
upper = item |> isUpper
alpha = item |> isAlpha
space = item |> isSpace
digit = item |> isDigit
other = item |> (\c -> (not $ isAlpha c) && (not $ isDigit c) && (not $ isSpace c))

{-# INLINE satisfy #-}
satisfy :: MonadParser Char m => (Char -> Bool) -> m Char
satisfy a = item |> a

{-# INLINE lexical #-}
lexical :: (MonadPlus m,MonadParser Char m) => m a -> m a
lexical p = do
	a <- p
	many space
	return a

------------------------------------------------------------------------------

type Parser a = (Functor m,MonadPlus m,MonadParser Char m) => m a

