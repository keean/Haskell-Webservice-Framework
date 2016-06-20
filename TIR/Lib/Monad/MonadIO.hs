{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.Monad.MonadIO where

import Control.Monad hiding (guard)
import Control.Concurrent
import Control.Exception as Exception
import Lib.Monad.MonadT 
import IO
import Network

------------------------------------------------------------------------------

class Monad m => MonadIO m where
	ioPrint :: Show a => a -> m ()
	ioHFlush :: Handle -> m ()
	ioPutChar :: Char -> m ()
	ioHPutChar :: Handle -> Char -> m ()
	ioPutStr :: String -> m ()
	ioPutStrLn :: String -> m ()
	ioHPutStr :: Handle -> String -> m ()
	ioGetChar :: m Char
	ioHGetChar :: Handle -> m Char
	ioGetLine :: m String
	ioHGetLine :: Handle -> m String
	ioGetContents :: m String
	ioHGetContents :: Handle -> m String
	ioThreadDelay :: Int -> m ()
	ioHClose :: Handle -> m ()
	ioConnectTo :: HostName -> PortID -> m Handle
	ioHSetBuffering :: Handle -> BufferMode -> m ()
	ioCatch :: m a -> (Exception -> m a) -> m a
	ioBracket :: m a -> (a -> m b) -> (a -> m c) -> m c
	ioThrow :: Exception -> m a
	ioUserException :: String -> m a
	liftIO :: IO a -> m a

instance MonadIO IO where
	ioPrint = print
	ioHFlush = hFlush
	ioPutChar = putChar
	ioHPutChar = hPutChar
	ioPutStr = putStr
	ioPutStrLn = putStrLn
	ioHPutStr = hPutStr
	ioGetChar = getChar
	ioHGetChar = hGetChar
	ioGetLine = getLine
	ioHGetLine = hGetLine
	ioGetContents = getContents
	ioHGetContents = hGetContents
	ioThreadDelay = threadDelay
	ioHClose = hClose
	ioConnectTo = connectTo
	ioHSetBuffering = hSetBuffering
	ioCatch = Exception.catch 
	ioBracket = Exception.bracket
	ioThrow = Exception.throw
	ioUserException = Exception.throw . IOException . userError
	liftIO = id

instance (MonadIO m,MonadT t m) => MonadIO (t m) where
	ioPrint = up . ioPrint
	ioHFlush = up . ioHFlush
	ioPutChar = up . ioPutChar 
	ioHPutChar h = up . ioHPutChar h
	ioPutStr = up . ioPutStr
	ioPutStrLn = up . ioPutStrLn
	ioHPutStr h = up . ioHPutStr h
	ioGetChar = up ioGetChar
	ioHGetChar = up . ioHGetChar
	ioGetLine = up ioGetLine
	ioHGetLine = up . ioHGetLine
	ioGetContents = up ioGetContents
	ioHGetContents = up . ioHGetContents
	ioThreadDelay = up . ioThreadDelay
	ioHClose = up . ioHClose
	ioConnectTo h = up . ioConnectTo h
	ioHSetBuffering h = up . ioHSetBuffering h
	ioCatch = up2 ioCatch
	ioBracket = up3 ioBracket
	ioThrow = up . ioThrow
	ioUserException = up . ioUserException
	liftIO = up . liftIO

