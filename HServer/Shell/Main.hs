{-# OPTIONS -fglasgow-exts  -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- server.hs: Copyright (C)2002 Keean Schupke
--
--		HyperServer main module

module Main(main) where

import Prelude hiding (catch)
import Socket
import Data.FiniteMap
import Directory
import Control.Exception (catch,finally)
import IO hiding (catch)
import System
import Control.Monad.Error

import Lib.Monad.MonadParser
import Lib.Linker.Linker
import Lib.Parser.Parser
import Lib.HTTP.Types

type Hierarchy = [String]
type Path = String
type Module = (Hierarchy,Path)

serverObjects :: String -> [String]
serverObjects libPath = [
	libPath++"HSbase",libPath++"HSbase_cbits",
	libPath++"HSutil",libPath++"HSutil_cbits",
	libPath++"HSlang",libPath++"HSlang_cbits",
	libPath++"HSposix",libPath++"HSposix_cbits",
	libPath++"HSdata",libPath++"HSconcurrent",libPath++"HSnet",
	libPath++"HSnetwork",libPath++"HStext",libPath++"HShaskell98",
	"Lib/Data/Version",
	"Lib/Monad/MonadT","Lib/Monad/MonadParser","Lib/Monad/ParserT","Lib/Monad/MonadState","Lib/Monad/MonadControl",
	"Lib/Monad/MonadIO","Lib/Monad/BacktrT",
	"Lib/Parser/Parser",
 	"Lib/XML/Types","Lib/XML/Parser","Lib/XML/MonadDom","Lib/XML/Dom","Lib/XML/DomT","Lib/XML/Generator",
 	"Lib/HTML/Types","Lib/HTML/DOM","Lib/HTML/MonadHtml","Lib/HTML/HtmlFragment","Lib/HTML/HtmlFragmentT",
	"Lib/HTML/Colours","Lib/HTML/MenuBar",
	"Lib/HTTP/Types","Lib/HTTP/Headers","Lib/HTTP/Parser","Lib/HTTP/Server","Lib/HTTP/Client","Lib/HTTP/Figo",
	"Lib/HTTP/Bookamove",
	"Lib/DBC/HIODBC","Lib/DBC/HIODBC_cbits","Lib/DBC/HSQL","Lib/DBC/Query",
	"Lib/Server/Types","Lib/Server/Error",
	"Server/HServer",
	"Lib/MetaSearch/MetaSearch","Lib/MetaSearch/parse","Lib/MetaSearch/dom","Lib/MetaSearch/filter",
	"Lib/MetaSearch/forest","Lib/MetaSearch/forms","Lib/MetaSearch/cookies","Lib/MetaSearch/shuffle",
	"Lib/MetaSearch/redirect"]


main :: IO ()
main = do -- load libraries and server
	socket <- listenOn (PortNumber 8000)
	linkerInit
	libPath <- getEnv ("HLIBPATH")
	loadArch (serverObjects $ libPath++"/")
	serverLoop socket 

serverLoop :: Socket -> IO ()
serverLoop socket = do
	hPutStr stderr "Server Start\n"
	modules <- getDirectoryContents "Modules"
	appObjects <- getModules "Modules" modules
	loadObjs appObjects
	linkerResolveObjs
	setups <- linkObjs appObjects 
	serverRun socket setups
	unloadObjs appObjects
	serverLoop socket

serverRun :: Socket -> [IO Application] -> IO ()
serverRun socket setups = do
	(maybeServerMain :: Maybe (Socket -> Server)) <- linkerLookupSymbol ["Server","HServer"] "serverMain"
	case maybeServerMain	of
		Nothing -> hPutStr stderr "could not lookup serverMain\n"
		Just serverMain -> do
			serverList <- startAll setups
			serverMain socket (listToFM serverList)
			hPutStr stderr "Server exited normally... sending term\n"
			hPutStr stderr "waiting for threads...\n"
			stopAll serverList
			hPutStr stderr "all done\n"
	`catch` (\e -> hPutStr stderr ((shows e) "\n"))

----------------------------------------------------------------

startAll :: [IO Application] -> IO [Application]
startAll  [] = return []
startAll (fn:fns) = do
	j0 <- fn
	startApp j0 -- call start method
	js <- startAll fns
	return (j0:js)

stopAll :: [Application] -> IO ()
stopAll [] = return ()
stopAll (fn:fns) = do
	stopApp fn
	js <- stopAll fns
	return ()
	
getModules :: String -> [String] -> IO [Module]
getModules _ [] = return []
getModules dir (m0:ms) = do
	case m0 of
		"." -> getModules dir ms
		".." -> getModules dir ms
		"makefile" -> getModules dir ms
		"makefile.in" -> getModules dir ms
		"CVS" -> getModules dir ms
		_ -> do 
			files <- getDirectoryContents (dir++('/':m0))
			case filterFiles dir m0 files of
				[] -> getModules dir ms
				objs -> do
					more <- getModules dir ms
					return (objs++more)

-- file list -> initialiser list
linkObjs :: [Module] -> IO [IO Application]
linkObjs [] = return []
linkObjs ((f,_):fs) = do
	link <- linkerLookupSymbol f "initialise"
	case link of
		Nothing -> linkObjs fs
		Just (obj :: IO Application) -> do
			objs <- linkObjs fs
			return (obj:objs)
	`catch` (\e -> do
		hPutStr stderr (shows e "\n")
		objs <- linkObjs fs
		return objs)

-- load files from list
loadObjs :: [Module] -> IO ()
loadObjs [] = return ()
loadObjs ((_,f):fs) = do
	linkerLoadObj (showString f ".o")
	`catch` (\e -> hPutStr stderr (shows e "\n"))
	`finally` (loadObjs fs)

loadArch :: [String] -> IO ()
loadArch [] = return ()
loadArch (f:fs) = do
	linkerLoadObj (showString f ".o")
	`catch` (\e -> hPutStr stderr (shows e "\n"))
	`finally` (loadArch fs)

unloadArch :: [String] -> IO ()
unloadArch [] = return ()
unloadArch (f:fs) = do
	linkerUnloadObj (showString f ".o")
	`catch` (\e -> hPutStr stderr (shows e "\n"))
	`finally` (unloadArch fs)

unloadObjs :: [Module] -> IO ()
unloadObjs [] = return ()
unloadObjs ((_,f):fs) = do
	linkerUnloadObj (showString f ".o")
	`catch` (\e -> hPutStr stderr (shows e "\n"))
	`finally` (unloadObjs fs)

-- file list -> object file list
filterFiles :: String -> String -> [String] -> [Module]
filterFiles _ _ [] = []
filterFiles dir0 dir1 (f:fs) = case parse parseObjFile f of
	Just (_,p) -> ([dir0,dir1,p],dir0++('/':dir1)++('/':p)):filterFiles dir0 dir1 fs
	Nothing -> filterFiles dir0 dir1 fs

-- parser accepts files ending in .o
parseObjFile :: Parser String
parseObjFile = (do
	a <- untilP (do
		a <- literal item ".o"
		literal item ""
		return a) item
	return a)

