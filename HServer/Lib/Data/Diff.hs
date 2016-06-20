{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
-- String to String diff

module Lib.Data.Diff (distString,editDistance,diff,cyclicDiff,Op(..),Edit) where

import GHC.ST
import Data.Array.Unboxed
-- import Data.Array.MArray
import Data.Array.ST

data Op a = Copy a | Delete a | Insert a | Replace a a | DeleteTree a | InsertTree a |
	DeleteSwap a | InsertSwap a deriving Show
type Edit a = [Op a]

join :: [a] -> [a] -> [a]
join [] b = b
join (a0:as) b = a0:join as b

cyclicDiff :: Eq a => [a] -> [a] -> (Int,Edit a)
cyclicDiff [] [] = (0,[])
cyclicDiff a b = diff a (join b b)

type DynamicFn a = Bool -> [a] -> [a] -> (Int,Edit a) -> [(Int,Edit a)] -> [(Int,Edit a)] -> (Int,Edit a)

diff :: Eq a => [a] -> [a] -> (Int,Edit a)
diff a b = dynamic stringDiff a b

stringDiff :: Eq a => DynamicFn a
stringDiff isLower (a0:_) (b0:_) nw n w = if a0==b0
	then op nw (if isLower then Copy a0 else Copy b0)
	else inc $ min3 (op (head w) (if isLower then Delete b0 else Insert b0))
		(op nw (if isLower then Replace b0 a0 else Replace a0 b0))
		(op (head n) (if isLower then Insert a0 else Delete a0)) where

	inc :: (Int,Edit a) -> (Int,Edit a)
	inc (i,x) = (i+1,x)

	op :: (Int,Edit a) -> Op a -> (Int,Edit a)
	op (i,s) t = (i,t:s)

	min3 :: (Int,Edit a) -> (Int,Edit a) -> (Int,Edit a) -> (Int,Edit a)
	min3 (i,x) (j,y) (k,z) = if i < j
		then (i,x)
		else if j <= k
			then (j,y)
			else (k,z)

stringDiff _ _ _ _ _ _ = (0,[]) where

dynamic :: Eq a => DynamicFn a -> [a] -> [a] -> (Int,Edit a)
dynamic _ [] [] = (0,[])
dynamic dfn (a :: [a]) (b :: [a]) = last (if lab == 0
	then mainDiag
	else if lab>0
		then lowers !! (lab-1)
		else uppers !! (-lab-1)) where

	lab :: Int
	lab = (length a) - (length b)
	
	mainDiag :: [(Int,Edit a)]
	mainDiag = oneDiag False a b (0,[]) (head uppers) (head lowers)

	uppers :: [[(Int,Edit a)]]
	uppers = eachDiag False a b (mainDiag:uppers) 

	lowers :: [[(Int,Edit a)]]
	lowers = eachDiag True b a (mainDiag:lowers)

	oneDiag :: Bool -> [a] -> [a] -> (Int,Edit a) -> [(Int,Edit a)] -> [(Int,Edit a)] -> [(Int,Edit a)]
	oneDiag isLower c d firstelt diagAbove diagBelow = firstelt:doDiag c d firstelt diagAbove diagBelow where

		doDiag :: [a] -> [a] -> (Int,Edit a) -> [(Int,Edit a)] -> [(Int,Edit a)] -> [(Int,Edit a)]
		doDiag [] _ _ _ _ = []
		doDiag _ [] _ _ _ = []
		doDiag _ _ _ [] _ = []
		doDiag _ _ _ _ [] = []
		doDiag e f nw n w = me:(doDiag (tail e) (tail f) me (tail n) (tail w)) where

			me :: (Int,Edit a)
			me = dfn isLower e f nw n w
				
	eachDiag :: Bool -> [a] -> [a] -> [[(Int,Edit a)]] -> [[(Int,Edit a)]]
	eachDiag _ _ [] _ = []
	eachDiag _ _ _ [] = []
	eachDiag isLower c  (d0:ds) (lastDiag:diags) = (oneDiag isLower c ds firstelt nextDiag (tail lastDiag)):(eachDiag isLower c ds diags) where

		nextDiag :: [(Int,Edit a)]
		nextDiag = head (tail diags)

		firstelt :: (Int,Edit a)
		firstelt = (\(i,s) -> (i+1,if i>=0
			then case isLower of
				True -> Delete d0:s
				_ -> Insert d0:s
			else [])) (head lastDiag)

editDistance :: String -> String -> Int
editDistance [] [] = 0
editDistance a b = last (if lab == 0
	then mainDiag
	else if lab>0
		then lowers !! (lab-1)
		else uppers !! (-lab-1)) where

	min3 :: Int -> Int -> Int -> Int
	min3 x y z = if x < y
		then x
		else if y <= z
			then y
			else z

	lab :: Int
	lab = (length a) - (length b)
	
	uppers :: [[Int]]
	uppers = eachDiag a b (mainDiag:uppers) 

	lowers :: [[Int]]
	lowers = eachDiag b a (mainDiag:lowers)

	mainDiag :: [Int]
	mainDiag = oneDiag a b (head uppers) (-1:head lowers)

	oneDiag :: String -> String -> [Int] -> [Int] -> [Int]
	oneDiag (c :: [a]) (d :: [a]) diagAbove diagBelow = thisdiag where

		doDiag :: String -> String -> Int -> [Int] -> [Int] -> [Int]
		doDiag [] _ _ _ _ = []
		doDiag _ [] _ _ _ = []
		doDiag _ _ _ [] _ = []
		doDiag _ _ _ _ [] = []
		doDiag (a0:as) (b0:bs) nw n w = me:(doDiag as bs me (tail n) (tail w)) where

			me :: Int
			me = if a0==b0
				then nw
				else 1+min3 (head w) nw (head n)

		firstelt :: Int
		firstelt = 1+head diagBelow

		thisdiag :: [Int]
		thisdiag = firstelt:doDiag c d firstelt diagAbove (tail diagBelow) where

	eachDiag :: String -> String -> [[Int]] -> [[Int]]
	eachDiag _ [] _ = []
	eachDiag _ _ [] = []
	eachDiag c (_:ds) (lastDiag:diags) = (oneDiag c ds nextDiag lastDiag):(eachDiag c ds diags) where

		nextDiag :: [Int]
		nextDiag = head (tail diags)

distString :: String -> String -> Int
distString s0 s1 = runST (difST s0 s1) 

difST :: MArray (STUArray s) Int (ST s) => String -> String -> ST s Int
difST s0 s1 = do
	b@(_,br) <- return $ (\x1 y1 -> ((0,0),(x1,y1))) (length s0) (length s1)
	d <- newArray b 0 :: ST s (STUArray s (Int,Int) Int)
	mdiff d s0 s1
	readArray d br

mMin :: Int -> Int -> Int -> Int
mMin i j k = min (min i j) k

costDelete :: Char -> Int
costDelete _ = 1

costInsert :: Char -> Int
costInsert _ = 1

costReplace :: Char -> Char -> Int
costReplace _ _  = 1

mdiff :: MArray a Int m => a (Int,Int) Int -> String -> String -> m ()
mdiff (d :: a e i) s0 s1 = do
	writeArray d (0,0) 0
	foreach 1 s0 $ \x a -> do
		dx <- readArray d (x-1,0)
		writeArray d (x,0) (dx+costDelete a)
	foreach 1 s1 $ \y b -> do
		dy <- readArray d (0,y-1)
		writeArray d (0,y) (dy+costInsert b)
	foreach 1 s0 $ \x a -> do
		foreach 1 s1 $ \y b -> do
			dx <- readArray d (x-1,y)
			dy <- readArray d (x,y-1)
			dxy <- readArray d (x-1,y-1)
			writeArray d (x,y) $ mMin (dx+costDelete a) (dy+costInsert b)
				(if a==b then dxy else dxy+costReplace a b)
	where

	foreach :: MArray a Int m => Int -> String -> (Int -> Char -> m ()) -> m ()
	foreach _ [] _ = return ()
	foreach i (c0:cs) f = do
		f i c0
		foreach (i+1) cs f
		
