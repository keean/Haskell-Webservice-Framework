{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
-- String to String diff

module Lib.Data.XmlDiff2(xmlDiffSwap,debugDiffSwap,debugDiffSwap1) where

import GHC.ST
import GHC.Arr
import GHC.Int
import Data.Array.ST

import Lib.XML.Types
import Lib.XML.Generator
import Lib.Data.Diff

type PostorderArray = Array Int XmlElement
type KeyrootList = [Int]
type LeftArray = Array Int Int
type DiffTree = (PostorderArray,KeyrootList,LeftArray)

data ZSCode = ZSCopy Int | ZSDelete Int | ZSReplace Int Int | ZSInsert Int 
	| ZSDeleteTree Int | ZSInsertTree Int | ZSSwap Int Int deriving Show
type ZSResult = (Int,[ZSCode])

------------------------------------------------------------------------------

costDelete :: XmlElement -> Int
costDelete e = 1 -- length $ showElement e ""

costInsert :: XmlElement -> Int
costInsert e = 1 -- length $ showElement e ""

costReplace :: XmlElement -> XmlElement -> Int
costReplace a b = 1 -- editDistance (showElement a "") (showElement b "")
-- costReplace x y = distString (showElement x "") (showElement y "")

costDeleteTree :: XmlElement -> Int -> Int
costDeleteTree e n = 1 -- (length $ showElement e "")

costInsertTree :: XmlElement -> Int -> Int
costInsertTree e n = n -- (length $ showElement e "")

costSwap :: Int -> Int -> Int
costSwap i j = 1
------------------------------------------------------------------------------

xmlDiffSwap :: DOM -> DOM -> (Int,Edit XmlElement)
xmlDiffSwap d d' = (\k k' -> postProcess $ toEdit k k' $ diffResult k k') (keyroots d) (keyroots d')

debugDiffSwap :: DOM -> DOM -> (Int,[(Int,Int,Op XmlElement)])
debugDiffSwap d d' = (\k k' -> toEdit k k' $ diffResult k k') (keyroots d) (keyroots d')

debugDiffSwap1 :: DOM -> DOM -> Array (Int,Int) ZSResult
debugDiffSwap1 d d' = (\k k' -> diffMatrix k k') (keyroots d) (keyroots d')

------------------------------------------------------------------------------

keyroots :: DOM -> DiffTree
keyroots d = runST (keyST d)

keyST :: (MArray (STArray s) XmlElement (ST s),MArray (STUArray s) Int (ST s)) => DOM -> ST s DiffTree
keyST d = do
	n <- return (countTags d)
	a <- newArray (1,n) Undefined :: ST s (STArray s Int XmlElement)
	b <- newArray (1,n) 0 :: ST s (STUArray s Int Int)
	k <- dtoa a b 1 (-1) [] d
	t <- unsafeFreeze a
	l <- unsafeFreeze b
	return (t,k,l)

dtoa :: (MArray a XmlElement m,MArray b Int m) => a Int XmlElement -> b Int Int -> Int -> Int
	-> [(XmlElement,Int)] -> DOM -> m KeyrootList
dtoa _ _ _ _ [] [] = return []
dtoa a b i y [] ((_,e0@(STag _ _)):ds) = dtoa a b i y [(e0,i)] ds
dtoa a b i _ [] ((x,ETag _):ds) = dtoa a b i x [] ds
dtoa a b i y [] ((x,e0):ds) = do
	writeArray a i e0
	writeArray b i i
	kr <- dtoa a b (i+1) (x+1) [] ds
	return (if x <= y then (i:kr) else kr)
dtoa _ _ _ _ _ [] = return []
dtoa a b i y k ((_,e0@(STag _ _)):ds) = dtoa a b i y ((e0,i):k) ds
dtoa a b i y ((k0,j):ks) ((x,ETag _):ds) = do
	writeArray a i k0
	writeArray b i j
	kr <- dtoa a b (i+1) x ks ds
	return (if x <= y then (i:kr) else kr)
dtoa a b i y k ((x,e0):ds) = do
	writeArray a i e0
	writeArray b i i
	kr <- dtoa a b (i+1) (x+1) k ds
	return (if x <= y then (i:kr) else kr)

countTags :: DOM -> Int
countTags [] = 0
countTags ((_,ETag _):ds) = countTags ds
countTags (_:ds) = 1+countTags ds

------------------------------------------------------------------------------

diffMatrix :: DiffTree -> DiffTree -> Array (Int,Int) ZSResult
diffMatrix t0 t1 = runST (matrixST t0 t1)

matrixST :: MArray (STArray s) ZSResult (ST s) => DiffTree -> DiffTree -> ST s (Array (Int,Int) ZSResult)
matrixST t0@(p0,_,_) t1@(p1,_,_) = do
	b@(_,br) <- return $ (\(x0,x1) (y0,y1) -> ((x0,y0),(x1,y1))) (GHC.Arr.bounds p0) (GHC.Arr.bounds p1)
	d <- newArray b (0,[]) :: ST s (STArray s (Int,Int) (Int,[ZSCode]))
	kdiff d t0 t1
	unsafeFreeze d

diffResult :: DiffTree -> DiffTree -> ZSResult
diffResult t0 t1 = runST (difST t0 t1)

difST :: MArray (STArray s) ZSResult (ST s) => DiffTree -> DiffTree -> ST s ZSResult
difST t0@(p0,_,_) t1@(p1,_,_) = do
	b@(_,br) <- return $ (\(x0,x1) (y0,y1) -> ((x0,y0),(x1,y1))) (GHC.Arr.bounds p0) (GHC.Arr.bounds p1)
	d <- newArray b (0,[]) :: ST s (STArray s (Int,Int) (Int,[ZSCode]))
	kdiff d t0 t1
	readArray d br

kdiff :: MArray a ZSResult m => a (Int,Int) ZSResult -> DiffTree -> DiffTree -> m ()
kdiff (d :: a e i) (t0,k0,l0) (t1,k1,l1) = do
	foreach k0 $ \x -> do
		lx <- return (l0!x)
		x0 <- return (lx-1)
		foreach k1 $ \y -> do
			ly <- return (l1!y)
			y0 <- return (ly-1)
			(fd :: a e i) <- newArray ((x0,y0),(x,y)) (0,[])
			writeArray fd (x0,y0) (0,[])
			for lx x $ \i -> do
				li <- return (l0!i)
				(fdli,feli) <- readArray fd (li-1,y0)
				writeArray fd (i,y0) (fdli + costDeleteTree (t0!li) (1+i-li),(ZSDeleteTree i:feli))
			for ly y $ \j -> do
				lj <- return (l1!j)
				(fdlj,felj) <- readArray fd (x0,lj-1)
				writeArray fd (x0,j) (fdlj + costInsertTree (t1!lj) (1+j-lj),(ZSInsertTree j:felj))
			for lx x $ \i -> do
				ti <- return (t0!i)
				li <- return (l0!i)
				for ly y $ \j -> do
					tj <- return (t1!j)
					lj <- return (l1!j)
					(fdi,fei) <- readArray fd (i-1,j)
					(fdj,fej) <- readArray fd (i,j-1)
					(fdli,feli) <- readArray fd (li-1,j)
					(fdlj,felj) <- readArray fd (i,lj-1)
					m <- min4
						(fdi+costDelete ti,ZSDelete i:fei)
						(fdj+costInsert tj,ZSInsert j:fej)
						(fdli+costDeleteTree ti 1,ZSDeleteTree i:feli)
						(fdlj+costInsertTree tj (1+j-lj),ZSInsertTree j:felj)
					if lx == li && ly == lj
					   then do
						(fdk,fek) <- readArray fd (i-1,j-1)
						dij <- min2 m $ if ti == tj
							then (fdk,ZSCopy i:fek)
							else (fdk + costReplace ti tj,ZSReplace i j:fek)
						writeArray d (i,j) dij
						writeArray fd (i,j) dij
						else if isKeyroot i k0 && isKeyroot j k1
							then do
							(fdlk,felk) <- readArray fd (li-1,lj-1)
							(fdllk,fellk) <- readArray fd ((l0!li)-1,(l1!lj)-1)
							(dk,ek) <- readArray d (i,j)
							dij <- min3 m (dk+fdlk,ek++felk) (fdllk+costSwap i j,ZSSwap i j:fellk)
							writeArray fd (i,j) dij
							else do
							(fdlk,felk) <- readArray fd (li-1,lj-1)
							(dk,ek) <- readArray d (i,j)
							dij <- min2 m (dk+fdlk,ek++felk)
							writeArray fd (i,j) dij
	where

	isKeyroot :: Int -> [Int] -> Bool
	isKeyroot _ [] = False
	isKeyroot i (kr0:krs)
		| i==kr0 = True
		| otherwise = isKeyroot i krs

	foreach :: MArray a ZSResult m => [l] -> (l -> m ()) -> m ()
	foreach [] _ = return ()
	foreach (l:ls) f = do
		f l
		foreach ls f

	for :: MArray a ZSResult m => Int -> Int -> (Int -> m ()) -> m ()
	for i j f
		| i <= j = do
			f i
			for (i+1) j f
		| otherwise = return ()

	min2 :: MArray a ZSResult m => (Int,z) -> (Int,z) -> m (Int,z) 
	min2 x@(i,_) y@(j,_)
		| i<j = return x
		| otherwise = return y

	min3 :: MArray a ZSResult m => (Int,z) -> (Int,z) -> (Int,z) -> m (Int,z)
	min3 i j k = do
		a <- min2 i j
		min2 a k
	
	min4 :: MArray a ZSResult m => (Int,z) -> (Int,z) -> (Int,z) -> (Int,z) -> m (Int,z)
	min4 i j k l  = do
		a <- min2 i j
		b <- min2 k l
		min2 a b

------------------------------------------------------------------------------

toDOM :: [(Int,Int,Op XmlElement)] -> [(Int,Int,Op XmlElement)] -> Edit XmlElement
toDOM s0 [] = case s0 of
 	((_,_,p0):t0) ->  p0:toDOM t0 []
	_ -> []
toDOM s0 ol@(ps@(i,j,op):ops) = case s0 of
	((x,y,ox):t0) | (y>=j && x>=i) -> ox:toDOM t0 ol
	_ -> case op of
		(Copy (STag n _)) -> Copy (ETag n):toDOM (ps:s0) ops
		(Replace (STag n _) (STag m _)) -> Replace (ETag n) (ETag m):toDOM (ps:s0) ops
		(Replace (STag n a) t1) -> Replace (EmptyTag n a) t1:toDOM s0 ops
		(Replace t0 (STag m b)) -> Replace t0 (EmptyTag m b):toDOM s0 ops
		(Delete (STag n _)) -> Delete (ETag n):toDOM (ps:s0) ops
		(Insert (STag m _)) -> Insert (ETag m):toDOM (ps:s0) ops
		(DeleteTree (STag n _)) -> DeleteTree (ETag n):toDOM (ps:s0) ops
		(InsertTree (STag m _)) -> InsertTree (ETag m):toDOM (ps:s0) ops
		(DeleteSwap (STag n _)) -> DeleteSwap (ETag n):toDOM (ps:s0) ops
		(InsertSwap (STag m _)) -> InsertSwap (ETag m):toDOM (ps:s0) ops
		_ -> op:toDOM s0 ops

postDepthList :: DiffTree -> [Int]
postDepthList (p,_,l) = (\(j0,j1) -> pd 0 [j0] j1) (GHC.Arr.bounds p) where

	pd :: Int -> [Int] -> Int -> [Int]
	pd _ [] _ = []
	pd d j@(j0:js) j1 
		| j1>=j0 && j1>(l!j1) = d:pd (d+1) ((l!j1):j) (j1-1)
		| j1>=j0 = d:pd d j (j1-1)
		| otherwise = pd (d-1) js j1

postProcess :: (Int,[(Int,Int,Op XmlElement)]) -> (Int,Edit XmlElement)
postProcess (dif,zse) = (dif,toDOM [] zse)

toEdit :: DiffTree -> DiffTree -> ZSResult -> (Int,[(Int,Int,Op XmlElement)])
toEdit t0@(p0,_,l0) t1@(p1,_,l1) (dif,zse) = (dif,ztoe zse (postDepthList t0) (postDepthList t1)) where 

	ztoe :: [ZSCode] -> [Int] -> [Int] -> [(Int,Int,Op XmlElement)]
	ztoe (z0:zs) i@(i0:is) [] = case z0 of
		ZSCopy x -> (i0,-1,Copy (p0!x)):ztoe zs is []
		ZSDelete x -> (i0,-1,Delete (p0!x)):ztoe zs is []
		ZSReplace x y -> (i0,-1,Replace (p0!x) (p1!y)):ztoe zs is []
		ZSInsert y -> (i0,-1,Insert (p1!y)):ztoe zs i []
		ZSDeleteTree x -> zdelete i [] (l0!x) x zs
		ZSInsertTree y -> zinsert i [] (l1!y) y zs
		ZSSwap x y -> zswap i [] (l0!x) x (l1!y) y zs
	ztoe (z0:zs) [] j@(j0:js) = case z0 of
		ZSCopy x -> (-1,j0,Copy (p0!x)):ztoe zs [] js
		ZSDelete x -> (-1,j0,Delete (p0!x)):ztoe zs [] j
		ZSReplace x y -> (-1,j0,Replace (p0!x) (p1!y)):ztoe zs [] js
		ZSInsert y -> (-1,j0,Insert (p1!y)):ztoe zs [] js
		ZSDeleteTree x -> zdelete [] j x (l0!x) zs
		ZSInsertTree y -> zinsert [] j y (l1!y) zs 
		ZSSwap x y -> zswap [] j (l0!x) x (l1!y) y zs
	ztoe (z0:zs) i@(i0:is) j@(j0:js) = case z0 of
		ZSCopy x -> (i0,j0,Copy (p0!x)):ztoe zs is js
		ZSDelete x -> (i0,j0,Delete (p0!x)):ztoe zs is j
		ZSReplace x y -> (i0,j0,Replace (p0!x) (p1!y)):ztoe zs is js
		ZSInsert y -> (i0,j0,Insert (p1!y)):ztoe zs i js
		ZSDeleteTree x -> zdelete i j (l0!x) x zs
		ZSInsertTree y -> zinsert i j (l1!y) y zs
		ZSSwap x y -> zswap i j (l0!x) x (l1!y) y zs
	ztoe _ _ _ = []

	zdelete :: [Int] -> [Int] -> Int -> Int -> [ZSCode] -> [(Int,Int,Op XmlElement)]
	zdelete i@(i0:is) j@(j0:_) x y z
		| x<=y = (i0,j0,DeleteTree (p0!y)):zdelete is j x (y-1) z
		| otherwise = ztoe z i j
	zdelete i@(i0:is) [] x y z
		| x<=y = (i0,-1,DeleteTree (p0!y)):zdelete is [] x (y-1) z
		| otherwise = ztoe z i []
	zdelete [] j@(j0:_) x y z
		| x<=y = (-1,j0,DeleteTree (p0!y)):zdelete [] j x (y-1) z
		| otherwise = ztoe z [] j
	zdelete _ _ _ _ _ = []

	zinsert :: [Int] -> [Int] -> Int -> Int -> [ZSCode] -> [(Int,Int,Op XmlElement)]
	zinsert i@(i0:_) j@(j0:js) x y z
		| x<=y = (i0,j0,InsertTree (p1!y)):zinsert i js x (y-1) z
		| otherwise = ztoe z i j
	zinsert i@(i0:_) [] x y z
		| x<=y = (i0,-1,InsertTree (p1!y)):zinsert i [] x (y-1) z
		| otherwise = ztoe z i []
	zinsert [] j@(j0:js) x y z
		| x<=y = (-1,j0,InsertTree (p1!y)):zinsert [] js x (y-1) z
		| otherwise = ztoe z [] j
	zinsert _ _ _ _ _ = []

	zswap :: [Int] -> [Int] -> Int -> Int -> Int -> Int -> [ZSCode] -> [(Int,Int,Op XmlElement)]
	zswap i@(i0:is) j@(j0:js) x0 x1 y0 y1 z
		| x0<=x1 = (i0,j0,DeleteSwap (p0!x1)):zswap is j x0 (x1-1) y0 y1 z
		| y0<=y1 = (i0,j0,InsertSwap (p1!y1)):zswap i js x0 x1 y0 (y1-1) z
		| otherwise = ztoe z i j
	zswap i@(i0:is) [] x0 x1 y0 y1 z
		| x0<=x1 = (i0,-1,DeleteSwap (p0!x1)):zswap is [] x0 (x1-1) y0 y1 z
		| y0<=y1 = (i0,-1,InsertSwap (p1!y1)):zswap i [] x0 x1 y0 (y1-1) z
		| otherwise = ztoe z i []
	zswap [] j@(j0:js) x0 x1 y0 y1 z
		| x0<=x1 = (-1,j0,DeleteSwap (p0!x1)):zswap [] j x0 (x1-1) y0 y1 z
		| y0<=y1 = (-1,j0,InsertSwap (p1!y1)):zswap [] js x0 x1 y0 (y1-1) z
		| otherwise = ztoe z [] j
	zswap _ _ _ _ _ _ _ = []

