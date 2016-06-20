{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Main(main) where

import Char
import IO
import System (getArgs)
import Numeric (showInt)
import Data.Array

import Lib.Arrow.Runnable
import Lib.XML.Types
import Lib.XML.Parser
import Lib.XML.Generator
import Lib.HTML.DOM
import Lib.Data.Diff
import Lib.Data.XmlDiff
import Lib.Data.XmlDiff1
import Lib.Data.XmlDiff2
import Lib.Monad.MonadT
import Lib.Monad.MonadIO
import Lib.HTML.MonadHtml
import Lib.HTML.HtmlFragment
import Lib.HTML.HtmlFragmentT
import Lib.HTML.Filter
import Lib.HTTP.Types
import Lib.XML.Dom
import Lib.HTML.Diff

ioWrite :: MonadIO m => HtmlFragmentT m ()
ioWrite = do
	sd <- htmlPop
	doc <- return $ showDOM (sd []) ""
	ioPutStr doc
	ioHFlush stdout

putDiffHtml :: (DOM -> DOM -> (ZSDist,Edit XmlElement)) -> DOM -> DOM -> IO ()
putDiffHtml f a b = run $ do
	htmlDiffHtml f a b
	write (Response {write=ioWrite})

showMatrix :: Show a => Array (Int,Int) a -> ShowS
showMatrix a = m (bounds a) where

	m :: ((Int,Int),(Int,Int)) -> ShowS
	m ((x0,y0),(x1,y1)) = sm x0 y0 where
		
		sm :: Int -> Int -> ShowS
		sm i j
			| i<=x1 && j<=y1 = shows (a!(i,j)) . showChar ' ' . sm i (j+1)
			| i<=x1 = showChar '\n' . sm (i+1) y0
			| otherwise = showChar '\n'

main :: IO ()
main = do
	args <- getArgs
	case args of
		(a0:b0:_) -> do
			a <- openFile a0 ReadMode
			b <- openFile b0 ReadMode
			x <- fmap (init . xmlParsed . htmlToDOM) (hGetContents a)
			y <- fmap (init . xmlParsed . htmlToDOM) (hGetContents b)
			-- print (keyroots x)
	 		-- print (keyroots y)
			-- putStr $ showMatrix (debugDiffExt1 (domTidy x) (domTidy y)) ""
			-- print $ xmlDiff (domTidy x) (domTidy y)
			-- print $ explode $ domTidy x
			putDiffHtml xmlDiff (explode $ domTidy x) (explode $ domTidy y)
			-- putDiffHtml xmlDiffExt (domTidy x) (domTidy y)
			-- print $ debugDiffSwap (domTidy x) (domTidy y)
			-- putDiffHtml xmlDiffSwap (domTidy x) (domTidy y)
			-- putStr $ showMatrix d "\n"
			-- putStr $ showMatrix e "\n"
			-- putStr (showEdit d "\n")
			return ()
		_ -> hPutStr stderr "Usage: diff <file A> [<file B>]\n"

explode :: DOM -> DOM
explode [] = []
explode ((i,Text []):ds) = explode ds
explode ((i,Text (t0:ts)):ds) = case t0 of
	CharData [] -> explode ((i,Text ts):ds)
	CharData s -> (map (\w -> (i,Text [CharData w])) (strToWords s)) ++ explode ds
	_ -> (i,Text [t0]):explode ((i,Text ts):ds)
explode (d0:ds) = d0:explode ds

strToWords :: String -> [String]
strToWords "" = []
strToWords (c0:cs) = [c0]:strToWords cs

rmDepth :: [DomNode] -> [XmlElement]
rmDepth [] = []
rmDepth ((_,d0):ds) = d0:rmDepth ds

showDiff :: [XmlElement] -> [XmlElement] -> IO ()
showDiff docA docB = do
	-- ed <- return $ dist docA docB
	-- putStr $ (showString "edit distance = " . showInt ed) "\n"
	-- (dst,dif) <- return $ dist docA docB
	-- putStr "edit distance = "
	-- putStr $ showInt dst "\n"
	-- putStr $ shows docA "\n"
	-- putStr $ shows docB "\n"
	-- putStr $ shows dif "\n"
	-- ioHPutDOM stdout (editToDOM dif)
	putChar '\n'
	hFlush stdout

editToDOM :: Edit XmlElement -> DOM
editToDOM es = etod 0 (etox $ reverse es)

etod :: Int -> [XmlElement] -> DOM
etod _ [] = []
etod i (x0@(STag _ _):xs) = (i,x0):etod (i+1) xs
etod i (x0@(ETag _):xs) = (i,x0):etod (i-1) xs
etod i (x0:xs) = (i,x0):etod i xs

etox :: Edit XmlElement -> [XmlElement]
etox [] = []
etox (Copy e0:es) = e0:etox es
etox (Delete e0:es) = case e0 of
	(PI "META" _) -> e0:etox es
	_ -> etox es
etox (DeleteTree e0:es) = case e0 of
	(PI "META" _) -> e0:etox es
	_ -> etox es
etox (DeleteSwap e0:es) = case e0 of
	(PI "META" _) -> e0:etox es
	_ -> etox es
etox (Insert e0:es) = e0:etox es
etox (InsertTree e0:es) = e0:etox es
etox (InsertSwap e0:es) = e0:etox es
etox (Replace e0 e1:es) = case e0 of
	(PI "META" _) -> e1:e0:etox es
	_ -> e1:etox es

