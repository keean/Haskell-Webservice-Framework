module Lib.MetaSearch.Shuffle(shuffle) where

import Random
import Int
import GHC.IOBase (unsafeInterleaveIO)

data Tree a = Leaf a | Node !Int (Tree a) (Tree a) deriving Show

buildTree :: [a] -> Tree a
buildTree = growLevel . (map Leaf) where

	growLevel :: [Tree a] -> Tree a
	growLevel [node] = node
	growLevel l = growLevel (inner l) 

	inner :: [Tree a] -> [Tree a]
	inner [] = []
	inner [e] = [e]
	inner (e1:e2:rest) = join e1 e2 : inner rest

	join :: Tree a -> Tree a -> Tree a
	join l@(Leaf _) r@(Leaf _) = Node 2 l r
	join l@(Node i _ _) r@(Leaf _) = Node (i+1) l r
	join l@(Leaf _) r@(Node i _ _) = Node (i+1) l r
	join l@(Node i _ _) r@(Node j _ _) = Node (i+j) l r

shuffle1 :: [a] -> [Int] -> [a]
shuffle1 elements rseq = shuffle' (buildTree elements) rseq where

	shuffle' :: Tree a -> [Int] -> [a]
	shuffle' (Leaf e) [] = [e]
	shuffle' tree (r0:rs) =
		case extractTree r0 tree of	
			(b0,bs) -> b0 : shuffle' bs rs
	
	extractTree :: Int -> Tree a -> (a,Tree a)
	extractTree 0 (Node _ (Leaf e) r) = (e,r)
	extractTree 1 (Node 2 (Leaf l) (Leaf r)) = (r,Leaf l)
	extractTree n (Node c (Leaf l) r) = case extractTree (n-1) r of
		(e,r') -> (e,Node (c-1) (Leaf l) r')
	extractTree n (Node c l (Leaf e))
		| n+1 == c = (e,l)
	extractTree n (Node c l@(Node c1 _ _) r)
		| n < c1 = case extractTree n l of
			(e,l') -> (e,Node (c-1) l' r)
		| otherwise = case extractTree (n-c1) r of
			(e,r') -> (e,Node (c-1) l r')

randList :: Int -> IO [Int]
randList 1 = return []
randList n = do
	a0 <- getStdRandom $ randomR (0,n-1)
	as <- unsafeInterleaveIO $ randList (n-1)
	return (a0:as)

shuffle :: [a] -> IO [a]
shuffle s = do
	a <- randList $ length s
	return $ shuffle1 s a

