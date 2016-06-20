-- Haskell SQL module
-- Copyright (C) 2002 Keean Schupke

module Lib.Server.Types where

import IO
import Network
import Data.FiniteMap
import Control.Concurrent
import Numeric

stringToPort :: String -> PortNumber
stringToPort s = case readDec s of
	((a,_):_) -> fromIntegral (a :: Int)
	_ -> 8000

data Server = Server {
	serverPort :: PortNumber,
	serverInit :: IO ConnectionHandler
}

defaultInit :: IO ConnectionHandler
defaultInit = return (\_con _url -> return ())

server :: Server
server = Server {
	serverPort = 8000,
	serverInit = defaultInit
}

instance Show Server where
	showsPrec _ s = showString "{ serverPort = "
		. shows (serverPort s)
		. showString " }"

data Connection = Connection {
	clientLocalPort :: PortNumber,
	clientHostname :: HostName,
	clientPort :: PortNumber,
	clientHandle :: Handle
} deriving Show

type State = MVar (FiniteMap String String)

data StateCommand a = ReadState String (MVar (Maybe a))
	| WriteState String !a
	| DeleteState String
	| RemoveIdle
	| NotIdle String

type ResponseMap = FiniteMap String ConnectionHandler

type ConnectionHandler = Connection -> String -> IO ()
