{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

-- State.hs: Copyright (C)2003 Keean Schupke

module Lib.HTTP.State (Continuation(..),ContinuationChan,ContinuationList,
	ContinuationMap,makeContinuation,getContinuationChan,newKey,readState,
	writeState,deleteState,touchState) where

import Char
import IO
import Data.FiniteMap
import Control.Concurrent
import System.Random

import Lib.Server.Types

data Continuation a = Continuation {
	continuationIdle :: Bool,
	continuationFn :: Maybe a
	}

makeContinuation :: Continuation a
makeContinuation = Continuation {
	continuationIdle = False,
	continuationFn = Nothing
	}

type ContinuationList a = [(String,Continuation a)]
type ContinuationMap a = FiniteMap String (Continuation a)
type ContinuationChan a = Chan (StateCommand (Continuation a))

newKey :: IO String
newKey = do
	key <- newIntegerKey 85
	return key

newIntegerKey :: Int -> IO String
newIntegerKey i 
	| i>0 = do
		a0 <- getStdRandom $ randomR (0,63)
		as <- newIntegerKey (i-1)
		return (toBase64 a0:as)
	| otherwise = return []

toBase64 :: Int -> Char
toBase64 i
	| i>=0 && i<=25 = chr (ord 'A'+i)
	| i>=26 && i<=51 = chr (ord 'a'+(i-26))
	| i>=52 && i<=61 = chr (ord '0'+(i-52))
	| i==62 = '+'
	| i==63 = '/'
	| otherwise = '='

getContinuationChan :: IO (ContinuationChan a)
getContinuationChan = do
	cmdChannel <- newChan
	forkIO $ execCommand cmdChannel emptyFM
	forkIO $ idleTimer cmdChannel 1800
	return cmdChannel

execCommand :: ContinuationChan a -> ContinuationMap a -> IO ()
execCommand chan state = do
	cmd <- readChan chan
	case cmd of
		DeleteState key -> execCommand chan (delFromFM state key)
		WriteState key val -> execCommand chan (addToFM state key val)
		ReadState key mvr -> do
			putMVar mvr (lookupFM state key)
			execCommand chan state
		NotIdle key -> execCommand chan (case lookupFM state key of
			Just s -> addToFM state key (s {continuationIdle=False})
			Nothing -> state)
		RemoveIdle -> do
			remaining <- removeIdle $ fmToList state
			execCommand chan (listToFM remaining)

removeIdle :: ContinuationList a -> IO (ContinuationList a)
removeIdle [] = return []
removeIdle ((k0,v0):ts) = if continuationIdle v0
	then do
		rest <- removeIdle ts
		return rest
	else do
		rest <- removeIdle ts
		return $! (k0,v0 {continuationIdle = True}):rest
	
idleTimer :: ContinuationChan a -> Int -> IO ()
idleTimer chan delay = do
	threadDelay (1000000*delay)
	writeChan chan $! RemoveIdle
	idleTimer chan delay

readState :: ContinuationChan a -> String -> IO (Maybe a)
readState chan key = if key == "" then return Nothing else do
	r <- newEmptyMVar
	writeChan chan $! ReadState key r
	m <- takeMVar r
	case m of
		Just c -> return (continuationFn c)
		_ -> return Nothing

writeState :: ContinuationChan a -> String -> a -> IO ()
writeState chan key value = writeChan chan (WriteState key (makeContinuation {
	continuationFn=Just value,
	continuationIdle=False}))

deleteState :: ContinuationChan a -> String -> IO ()
deleteState chan key = writeChan chan (DeleteState key)

touchState :: ContinuationChan a -> String -> IO ()
touchState chan key = writeChan chan (NotIdle key)

