-- main.hs (C)2001 Keean Schupke
--
--		main program, uses DOM module 

module Main(main) where
import Char
import IO
import Lib.MetaSearch.Parser
import Lib.MetaSearch.DOM
import Lib.MetaSearch.Filter
import Lib.MetaSearch.Forest
import Lib.MetaSearch.Forms
import Lib.MetaSearch.Cookies 
import FiniteMap
import Lib.MetaSearch.Shuffle
import Int
import Lib.MetaSearch.Redirect
import System

main :: IO ()
main = do
	args <- getArgs
	process args

process :: [String] -> IO ()
process [] = return ()
process (a:as) = do
	fetch a
	process as

justServer :: Parser String
justServer = do
	stringNcs "http://"
	untilChar (=='/')

filterDates :: FilterDOM ()
filterDates = do
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	cut 1
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	cut 1
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	cut 1
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	select [3]
	(graft . elemIs) (MkElem (Tag,"TD",[]))

getDates :: FilterDOM [(String,String)]
getDates = do
	a <- getDate
	as <- getDates `mplus` return []
	return (a:as)

getDate :: FilterDOM (String,String)
getDate = do
	(from . elemIs) (MkElem (Tag,"SPAN",[]))
	a <- (moveExc . elemIs) (MkElem (Tag,"SPAN",[]))
	(from . elemIs) (MkElem (Tag,"SPAN",[]))
	b <- (moveExc . elemIs) (MkElem (Tag,"TD",[]))
	return (extractString a,extractString b)

showDates :: [(String,String)] -> IO ()
showDates [] = putStr "\n"
showDates ((n,v):ds) = do
	putStr (n++": "++v++"\n")
	showDates ds

fetch :: String -> IO ()
fetch cn = do
	sr <- getDocument (MkElem (Document,"http-get://www.companieshouse.gov.uk/info/dynaflip.cgi",[]))
	useServer (case parse justServer $ getRedirect sr of
		Consumed (Ok x _) -> x
		_ -> "www.companieshouse.gov.uk")
	where

	useServer :: String -> IO ()
	useServer sv = do
		cr <- getDocument (MkElem (Document,"http-get://"++sv++"/info/do_search.cgi",
			[MkAttribute ("cname",""),MkAttribute ("cnumb",cn),MkAttribute ("id",""),MkAttribute ("table","1")]))
		ch <- followRedirects "" cr []
		case filterDOM (do alter tidyDOM;filterDates;getDates) ch of
			DOMOut x _ -> showDates x
			DOMVoid -> hPutStr stderr "\n"

