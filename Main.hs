{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Main (main) where

import System
import Server.HServer
import IO as IO
import Control.Concurrent

import Lib.Monad.MonadState
import Lib.Monad.StateT
import Lib.Monad.MonadIO
import Lib.Server.Types
import Lib.XML.Types
import Lib.XML.Parser
import Lib.XML.Filter
import Lib.Linker.Linker

defaultConfig :: String
defaultConfig = "hserver.conf"

readConfig :: String -> IO DOM
readConfig f = do
	h <- IO.openFile f ReadMode
	c <- hGetContents h
	c `seq` hClose h
	return (xmlToDOM c)

xmlToServer :: (MonadLinker m,MonadIO m) => DOM -> m [Server]
xmlToServer ((_,e):ds) = do
	ini <- loadObject (lookupAttribute e "location")
		(lookupAttribute e "module") "getHandler"
	servers <- xmlToServer ds
	return $ Server {
		serverPort = stringToPort (lookupAttribute e "port"),
		serverInit = ini
	} : servers
xmlToServer _ = return []

loadLibraries  :: MonadLinker m => DOM -> m ()
loadLibraries ((_,e):ds) = do
	linkerLoadObj (lookupAttribute e "location")
	loadLibraries ds
loadLibraries _ = return ()

mFilter :: (MonadState DOM m,MonadLinker m,MonadIO m) => m [Server]
mFilter = do
	(update . graft) (sTag "SERVER")
	srv <- getState
	app <- (return . match (emptyTag "APPLICATION")) srv
	lib <- (return . match (emptyTag "LIBRARY")) srv
	loadLibraries lib
	s <- xmlToServer app
	ioPrint "resolving Links"
	linkerResolveObjs
	return s

loadObject :: (MonadLinker m,MonadIO m) => String -> String -> String -> m (IO ConnectionHandler)
loadObject f o g = do
	(linkerLoadObj f) `ioCatch` (\e -> do ioPutStrLn (show e);return ())
	link <- (do
		l <- linkerLookupSymbol o g
		return $ case l of
			Nothing -> defaultInit
			Just (obj :: IO ConnectionHandler) -> obj) `ioCatch` (\e -> do
				ioPutStrLn (show e); return defaultInit)
	return link

startServers :: [Server] -> IO ()
startServers (s0:ss) = do
	print $ serverPort s0
	if (not $ null ss) 
		then do
			tid <- forkIO $ do
				hPutStrLn stdout "forked new server"
				serverMain (serverPort s0) (serverInit s0)
				return ()
			hPutStrLn stdout (show tid)
			return ()
		else do
			hPutStrLn stdout "enter last server"
			serverMain (serverPort s0) (serverInit s0)
			return ()
	startServers ss
startServers _ = return ()

main :: IO ()
main = do
	a <- getArgs
	d <- case a of
		(a0:as) -> readConfig a0
		_ -> readConfig defaultConfig 
	(_,f) <- (runST mFilter) d
	startServers f

