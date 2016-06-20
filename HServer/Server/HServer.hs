-- server.hs: Copyright (C)2002 Keean Schupke
--
--		HyperServer main module

module Server.HServer (serverMain) where

import Data.Char
--import Contol.Monad.IO
import Network.Socket
import Control.Concurrent
import Control.Exception as Exception
import Control.Monad
import Data.Map

import Lib.HTTP.Types
import Lib.HTTP.Figo as Figo
-- import Lib.HTTP.Bookamove as Bookamove
import Lib.HTTP.Figo as Figo
import Lib.HTTP.Demo as Demo
import Lib.Server.Types
import System.Posix.Signals

-- handleError :: ResponseMap -> Connection -> String -> IO ()
-- handleError _ _ cmd = hPutStr stderr $ (showString "Protocol Error: " . shows cmd) "\n"

-- handleTest :: ResponseMap -> Connection -> String -> IO ()
-- handleTest _ connection cmd = figoHandler connection cmd
-- handleTest _ connection state cmd = bookamoveHandler connection cmd

connectionHandler :: Connection -> ConnectionHandler -> IO ()
connectionHandler connection app = (((do
	hPutStr stderr $ (showString "Connection from: " . shows (clientHostname connection) . showChar ':' .
		shows (clientPort connection)) "\n"
	command <- hGetContents (clientHandle connection)
	app connection command
	hPutStr stderr $ (showString "Closing connection: " . shows (clientHostname connection) . showChar ':' .
		shows (clientPort connection)) "\n") `Exception.catch` (\e -> do
	hPutStr stderr (shows e "\n")
	hPutStr stderr "caught thread exception\n")) `Exception.finally` (do
	hPutStr stderr "application finally\n"
	hClose (clientHandle connection)
	hPutStr stderr "finally: close done\n"))

forkHandler :: Connection -> ConnectionHandler -> IO ()
forkHandler connection app = do
	forkIO (connectionHandler connection app)
	return ()

acceptConnections :: ThreadId -> Socket -> PortNumber -> ConnectionHandler -> IO ()
acceptConnections srvThread socket sport app = do
	hPutStr stderr "accepting connections\n"
	(h,host,cport) <- accept socket
	hSetBuffering h (BlockBuffering Nothing)
	-- hSetBuffering h LineBuffering
	-- hSetBuffering h NoBuffering
	(forkHandler (Connection {clientLocalPort=sport,clientHostname=host,clientPort=cport,clientHandle=h}) app)
		`Exception.catch` (\e -> hPutStr stderr (shows e "\n"))
	acceptConnections srvThread socket sport app

serverMain :: PortNumber -> (IO ConnectionHandler) -> IO ()
serverMain port init = do
	(sport,socket) <- open port
	srvThread <- myThreadId
	hPutStr stderr $ (showString "serverMain listening on: " . shows sport) "\n"
	installHandler sigPIPE Ignore Nothing
	app <- init
	acceptConnections srvThread socket sport app
		`Exception.catch` (\e -> do
			hPutStr stderr (showString "caught server exception: " . shows e $ " \n")
			return ())

open :: PortNumber -> IO (PortNumber,Socket)
open p = do
	do {s <- listenOn (PortNumber p); return (p,s)}
	`Exception.catch` (\_ -> open (p+1))

