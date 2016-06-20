{-# OPTIONS -fglasgow-exts #-}
-- {-# OPTIONS -fallow-undecidable-instances #-}

module Main where

import Char
import GHC.IOBase
import System.IO
import Data.Word
import Data.Array.IO
import Data.Array.Unboxed

data IList a i e = ICons i i (a i e) (IList a i e) | INil

showIList :: (IArray a e,Ix i,Show e) => IList a i e -> ShowS
showIList (ICons _ _ a l) = shows (elems a) . showIList' l
showIList (INil) = id

showIList' :: (IArray a e,Ix i,Show e) => IList a i e -> ShowS
showIList' (ICons _ _ a l) = showString " ++: " . shows (elems a) . showIList' l
showIList' (INil) = id 

instance (IArray a e,Ix i,Show e) => Show (IList a i e) where
	showsPrec _ l = showIList l

class List l e where
	nil :: l e
	null :: l e -> Bool
	head :: l e -> e
	tail :: l e -> l e
	(+:) :: e -> l e -> l e

class List (l a i) e => ListPlus l a i e where
	(++:) :: a i e -> l a i e -> l a i e
	part :: a i e -> i -> l a i e -> l a i e

infixr 9 +:
infixr 9 ++:

instance List [] e where
	nil = []
	null (_:_) = False
	null _ = True
	head (a:_) = a
	head _ = error "head: empty list"
	tail (_:l) = l
	tail _ = error "tail: empty list"
	a +: l = a:l

instance (IArray a e,Ix i,Num i) => List (IList a i) e where 
	nil = INil
	null INil = True
	null _ = False
	head (ICons i _ a _) = a!i
	head _ = error "head: empty list"
	tail (ICons i j a l)
		| i < j = ICons (i+1) j a l
		| otherwise = l
	tail _ = error "tail: empty list"
	a +: l = ICons 0 0 (array (0,0) [(0,a)]) l

instance (IArray a e,Ix i,Num i) => ListPlus IList a i e where
	a ++: l
		| e >= s = ICons s e a l
		| otherwise = l
		where ~(s,e) = bounds a
	part a i l
		| e >= i = ICons s i a l
		| otherwise = l
		where ~(s,e) = bounds a

hGetIList :: ListPlus l UArray Int Word8 => Int -> Handle -> IO (l UArray Int Word8)
hGetIList bufSize h = do
	mt <- newArray_ (0,bufSize-1)
	ioLoop mt 
	where

		ioLoop mt = unsafeInterleaveIO $ do
			sz <- hGetArray h mt bufSize
			hd <- freeze mt
			case sz of
				0 -> return nil
				n	| n < bufSize -> do
						return (part hd (n-1) nil)
					| otherwise -> do
						tl <- ioLoop mt
						return (hd ++: tl)

main :: IO ()
main = do
	(l :: IList UArray Int Word8) <- hGetIList 4096 stdin
	print $ wc l ' ' 0 0 0

wc :: List l Word8 => l Word8 -> Char -> Int -> Int -> Int -> (Int,Int,Int)
wc l p i j k
	| p `seq` i `seq` j `seq` k `seq` False = undefined
	| not $ Main.null l, h <- (toEnum . fromEnum . Main.head) l, t <- Main.tail l = case isSpace h of
		False -> wc t h (i + 1) (j + if isSpace p then 1 else 0) k
		_ -> wc t h (i + 1) j (k + if h == '\n' then 1 else 0)
	| otherwise = (i,j,k)
	
