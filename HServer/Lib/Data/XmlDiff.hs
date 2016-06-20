{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
-- String to String diff

module Lib.Data.XmlDiff (gtest,xmlDiff,debugDiff,debugDiff1,ZSDist) where

import GHC.ST
import GHC.Arr
import Data.Array.ST

import Lib.XML.Types
import Lib.Data.Diff

type PostorderArray = Array Int XmlElement
type KeyrootList = [Int]
type LeftArray = Array Int Int
type DiffTree = (PostorderArray,KeyrootList,LeftArray)

data ZSCode = ZSCopy Int Int | ZSDelete Int Int | ZSReplace Int Int | ZSInsert Int Int deriving Show
type ZSDist = Int
type ZSResult = (ZSDist,[ZSCode])

------------------------------------------------------------------------------

{-# INLINE costDelete #-}
costDelete :: ZSDist
costDelete = 1

{-# INLINE costInsert #-}
costInsert :: ZSDist
costInsert = 1

{-# INLINE costReplace #-}
costReplace :: ZSDist
costReplace = 1

------------------------------------------------------------------------------

xmlDiff :: DOM -> DOM -> (ZSDist,Edit XmlElement)
xmlDiff d d' = (\k k' -> postProcess $ toEdit k k' $ diffMatrix k k') (keyroots d) (keyroots d')

debugDiff :: DOM -> DOM -> (ZSDist,[(Int,Int,Op XmlElement)])
debugDiff d d' = (\k k' -> toEdit k k' $ diffMatrix k k') (keyroots d) (keyroots d')

debugDiff1 :: DOM -> DOM -> ZSResult
debugDiff1 d d' = (\k k' -> diffMatrix k k') (keyroots d) (keyroots d')

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

diffMatrix :: DiffTree -> DiffTree -> ZSResult
diffMatrix t0 t1 = runST (difST t0 t1)

{-# INLINE difST #-}
difST :: MArray (STArray s) ZSResult (ST s) => DiffTree -> DiffTree -> ST s ZSResult
difST t0@(p0,_,_) t1@(p1,_,_) = do
	b@(_,br) <- return $ (\(x0,x1) (y0,y1) -> ((x0,y0),(x1,y1))) (GHC.Arr.bounds p0) (GHC.Arr.bounds p1)
	d <- newArray b (0,[]) :: ST s (STArray s (Int,Int) ZSResult)
	kdiff d t0 t1
	readArray d br

{-# INLINE kdiff #-}
kdiff :: MArray a ZSResult m => a (Int,Int) ZSResult -> DiffTree -> DiffTree -> m ()
kdiff (d :: a e i) (t0,k0,l0) (t1,k1,l1) = do
	foreach k0 $ \x -> do
		lx <- return (l0!x)
		x0 <- return (lx-1)
		foreach k1 $ \y -> do
			ly <- return (l1!y)
			y0 <- return (ly-1)
			(fd :: a e i) <- newArray ((x0,y0),(x,y)) (0,[])
			-- writeArray fd (x0,y0) (0,[])
			for lx x $ \i -> do
				(fdi,fei) <- readArray fd (i-1,y0)
				writeArray fd (i,y0) (fdi + costDelete,(ZSDelete i y0:fei))
			for ly y $ \j -> do
				(fdj,fej) <- readArray fd (x0,j-1)
				writeArray fd (x0,j) (fdj + costInsert,(ZSInsert x0 j:fej))
			for lx x $ \i -> do
				li <- return (l0!i)
				for ly y $ \j -> do
					lj <- return (l1!j)
					(fdi,fei) <- readArray fd (i-1,j)
					(fdj,fej) <- readArray fd (i,j-1)
					m <- min2 (fdi + costDelete,ZSDelete i j:fei) (fdj + costInsert,ZSInsert i j:fej)
					if lx == li && ly == lj
						then do
							(fdk,fek) <- readArray fd (i-1,j-1)
							dij <- min2 m $ if (t0!i) == (t1!j)
								then (fdk,ZSCopy i j:fek)
								else (fdk + costReplace,ZSReplace i j:fek)
							writeArray d (i,j) dij
							writeArray fd (i,j) dij
						else do
							(fdk,fek) <- readArray fd (li-1,lj-1)
							(dk,ek) <- readArray d (i,j)
							dij <- min2 m (dk+fdk,ek++fek)
							writeArray fd (i,j) dij
	where

	{-# INLINE foreach #-}
	foreach :: MArray a ZSResult m => [l] -> (l -> m ()) -> m ()
	foreach [] _ = return ()
	foreach (l:ls) f = do
		f l
		foreach ls f

	{-# INLINE for #-}
	for :: MArray a ZSResult m => Int -> Int -> (Int -> m ()) -> m ()
	for i j f
		| i <= j = do
			f i
			for (i+1) j f
		| otherwise = return ()

	{-# INLINE min2 #-}
	min2 :: MArray a ZSResult m => (ZSDist,z) -> (ZSDist,z) -> m (ZSDist,z) 
	min2 x@(i,_) y@(j,_)
		| i<j = return x
		| otherwise = return y

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
		_ -> op:toDOM s0 ops

-- toDOM :: [(Int,Int,Op XmlElement)] -> [(Int,Int,Op XmlElement)] -> Edit XmlElement
-- toDOM s0 [] = case s0 of
--  	((_,_,p0):t0) ->  p0:toDOM t0 []
-- 	_ -> []
-- toDOM s0 ol@(ps@(i,j,op):ops) = case s0 of
	-- ((x,y,ox):t0) | (y>=j && j>=0) || (x>=i && x>=0) -> ox:toDOM t0 ol
	-- ((_,y,oj@(Insert _)):t0) | y>=j && j>=0 -> oj:toDOM t0 ol
	-- ((x,y,ox@(Copy _)):t0) | (y>=j && j>=0) || (x>=i && i>=0) -> ox:toDOM t0 ol
	-- ((x,y,ox@(Replace _ _)):t0) | (y>=j && j>=0) || (x>=i && i>=0) -> ox:toDOM t0 ol
	-- ((x,_,oi@(Delete _)):t0) | x>=i && i>=0 -> oi:toDOM t0 ol
	-- _ -> case op of
		-- (Copy (STag n _)) -> Copy (ETag n):toDOM (ps:s0) ops
		-- (Copy _) -> op:toDOM s0 ops
		-- (Replace (STag n _) (STag m _)) -> Replace (ETag n) (ETag m):toDOM (ps:s0) ops
		-- (Replace (STag n a) t1) -> Replace (EmptyTag n a) t1:toDOM s0 ops
		-- (Replace t0 (STag m b)) -> Replace t0 (EmptyTag m b):toDOM s0 ops
		-- (Replace _ _) -> op:toDOM s0 ops
		-- (Delete (STag n _)) -> Delete (ETag n):toDOM (ps:s0) ops
		-- (Delete _) -> op:toDOM s0 ops
		-- (Insert (STag m _)) -> Insert (ETag m):toDOM (ps:s0) ops
		-- (Insert _) -> op:toDOM s0 ops
		-- _ -> op:toDOM s0 ops

postDepthList :: DiffTree -> [Int]
postDepthList (p,_,l) = (\(j0,j1) -> pd 0 [j0] j1) (GHC.Arr.bounds p) where

	pd :: Int -> [Int] -> Int -> [Int]
	pd _ [] _ = []
	pd d j@(j0:js) j1 
		| j1>=j0 && j1>(l!j1) = d:pd (d+1) ((l!j1):j) (j1-1)
		| j1>=j0 = d:pd d j (j1-1)
		| otherwise = pd (d-1) js j1

postProcess :: (ZSDist,[(Int,Int,Op XmlElement)]) -> (ZSDist,Edit XmlElement)
postProcess (dif,zse) = (dif,toDOM [] zse)

toEdit :: DiffTree -> DiffTree -> ZSResult -> (ZSDist,[(Int,Int,Op XmlElement)])
toEdit t0@(p0,_,_) t1@(p1,_,_) (dif,zse) = (dif,ztoe zse (postDepthList t0) (postDepthList t1)) where 

	ztoe :: [ZSCode] -> [Int] -> [Int] -> [(Int,Int,Op XmlElement)]
	ztoe (z0:zs) i@(i0:is) [] = case z0 of
		(ZSCopy x _) -> (i0,-1,Copy (p0!x)):ztoe zs is []
		(ZSDelete x _) -> (i0,-1,Delete (p0!x)):ztoe zs is []
		(ZSReplace x y) -> (i0,-1,Replace (p0!x) (p1!y)):ztoe zs is []
		(ZSInsert _ y) -> (i0,-1,Insert (p1!y)):ztoe zs i []
	ztoe (z0:zs) [] j@(j0:js) = case z0 of
		ZSCopy x _ -> (-1,j0,Copy (p0!x)):ztoe zs [] js
		ZSDelete x _ -> (-1,j0,Delete (p0!x)):ztoe zs [] j
		ZSReplace x y -> (-1,j0,Replace (p0!x) (p1!y)):ztoe zs [] js
		ZSInsert _ y -> (-1,j0,Insert (p1!y)):ztoe zs [] js
	ztoe (z0:zs) i@(i0:is) j@(j0:js) = case z0 of
		ZSCopy x _ -> (i0,j0,Copy (p0!x)):ztoe zs is js
		ZSDelete x _ -> (i0,j0,Delete (p0!x)):ztoe zs is j
		ZSReplace x y -> (i0,j0,Replace (p0!x) (p1!y)):ztoe zs is js
		ZSInsert _ y -> (i0,j0,Insert (p1!y)):ztoe zs i js
	ztoe _ _ _ = []

------------------------------------------------------------------------------

data CdagNode a = Node [Int] | Leaf a deriving Show
type CDAG a = [CdagNode a]

test1 :: String
test1 = "<HTML><BODY><H1>test</H1>string</BODY></HTML>"

test2 :: String
test2 = "<HTML><BODY><H1>testing</H1>st<B>ri</B>ng</BODY></HTML>"

join :: String -> String -> String
join [] a = a
join (c0:cs) a = c0:join cs a

isContainedInString :: String -> Char -> Bool
isContainedInString [] _ = False
isContainedInString (c0:cs) c
	| c0==c = True
	| otherwise = isContainedInString cs c

uniqueString :: String -> String -> String
uniqueString a [] = reverse a
uniqueString a (c0:cs)
	| isContainedInString a c0 = uniqueString a cs
	| otherwise = uniqueString (c0:a) cs

toCDAG :: String -> CDAG Char
toCDAG [] = []
toCDAG (c0:cs) = (Leaf c0):toCDAG cs

stringToIndex :: String -> String -> [Int]
stringToIndex [] _ = []
stringToIndex _ [] = []
stringToIndex s (c0:cs) = indexOf (length s - 1) c0 s:stringToIndex s cs

indexOf :: Int -> Char -> String -> Int
indexOf _ _ [] = -1
indexOf i c (c0:cs)
	| c==c0 = i
	| otherwise = indexOf (i-1) c cs

addListToCDAG :: [Int] -> CDAG a -> CDAG a
addListToCDAG [] cdag = cdag
addListToCDAG is [] = [Node is]
addListToCDAG is cdag = Node is:cdag

-- isNodeInCDAG :: [Int] -> CDAG a -> Bool
-- isNodeInCDAG [] _ = False
-- isNodeInCDAG _ [] = False
-- isNodeInCDAG (i0:is) (c0:cs) = 

-- addNodeToCDAG :: [Int] -> CDAG a -> CDAG a

gtest :: IO ()
gtest = do
	u <- return $ uniqueString [] $ join test1 test2
	u0 <- return $ toCDAG u
	i0 <- return $ length u0
	u1 <- return $ addListToCDAG (stringToIndex u "<H1>test</H1>") u0
	i1 <- return $ i0 + 1
	u2 <- return $ addListToCDAG (stringToIndex u "<H1>testing</H1>") u1
	i2 <- return $ i1 + 1
	u3 <- return $ addListToCDAG (stringToIndex u "<B>ri</B>") u2
	i3 <- return $ i2 + 1
	u4 <- return $ addListToCDAG ((stringToIndex u "<BODY>")++(i1:stringToIndex u "string</BODY>")) u3
	i4 <- return $ i3 + 1
	u5 <- return $ addListToCDAG ((stringToIndex u "<BODY>")++(i2:stringToIndex u "st")++(i3:stringToIndex u "ng</BODY>")) u4
	i5 <- return $ i4 + 1
	u6 <- return $ addListToCDAG ((stringToIndex u "<HTML>")++(i4:stringToIndex u "string</HTML>")) u5
	i6 <- return $ i5 + 1
	u7 <- return $ addListToCDAG ((stringToIndex u "<HTML>")++(i5:stringToIndex u "string</HTML>")) u6
	i7 <- return $ i6 + 1
	print u7
	print i7

