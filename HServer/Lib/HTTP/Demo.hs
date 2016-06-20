{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- server.hs: Copyright (C)2002 Keean Schupke
--
--		HyperServer main module

module Lib.HTTP.Demo (getHandler) where

import GHC.IOBase
import Char
import IO
import Time
import Data.FiniteMap
-- import Data.Set
import Control.Monad
import GHC.Handle
import Numeric
import Control.Concurrent
import Control.Exception as Exception
----  import System.Random

import Lib.Arrow.Runnable
import Lib.Monad.MonadT
import Lib.Monad.MonadIO
import Lib.XML.Types
import Lib.HTML.Types
import Lib.HTML.MonadHtml
import Lib.HTML.HtmlFragmentT
import Lib.HTTP.Types
import Lib.HTTP.Server
-- import Lib.HTTP.Client
import Lib.Server.Types
import Lib.HTTP.State
import Lib.XML.MonadDom
import Lib.XML.DomT
import Lib.Monad.StateT
import Lib.Monad.MonadState

data DemoState = DemoState {
	stateKey :: String,
	stateCounter :: Integer,
	stateParams :: UriParameters
} deriving Show

makeDemoState :: DemoState
makeDemoState = DemoState {
	stateKey = "",
	stateCounter = 0,
	stateParams = UriParameters emptyFM
}

data DS = DS {
	stateDemoState::DemoState,
	stateRequest::HttpRequest,
	stateResponse::Response
}

newtype DemoContinuation = DC {runDC :: StateT DS (HtmlFragmentT IO) (Maybe DemoContinuation)} 

------------------------------------------------------------------------------

getHandler :: IO ConnectionHandler
getHandler = do
	cmdChannel <- getContinuationChan
	return (httpHandler (demoHandler cmdChannel))

demoHandler :: ContinuationChan (Maybe DemoContinuation,DemoState) -> HttpHandler
demoHandler stateChannel req _ = case uriPath (requestURI req) of
	UriPath u | u=="/" || u=="/index.html" || u=="/index.htm"  -> do
		respondHTML req (\rsp -> run (demo req rsp stateChannel)) HttpOk
	UriPath "/logout.html" -> do
		deleteState stateChannel (lookupDict (requestURI req) "session")
		respondRedirect req "/" 
	_ -> respondHTML req (\rsp -> run (htmlNotFound req rsp)) HttpNotFound

demo :: HttpRequest -> Response -> ContinuationChan (Maybe DemoContinuation,DemoState) -> HtmlFragmentT IO ()
demo req rsp stateChannel = do
	maybeState <- up $ readState stateChannel (lookupDict (requestURI req) "session")
	newContState@(_,newState) <- case maybeState of
		Just (Just cont,state) -> do
			up $ print "Just (Just cont,state)"
			key <- up $ newKey
			newState <- return $ state {
				stateKey=key,
				stateCounter=(stateCounter state+1),
				stateParams=uriParameters (requestURI req)}
			newCont <- run (runDC cont) DS {stateDemoState=newState,stateRequest=req,stateResponse=rsp}
			return (newCont,newState)
		Just (Nothing,state) -> do
			up $ print "Just (Nothing,state)"
			key <- up $ newKey
			newState <- return $ state {
				stateKey=key,
				stateCounter=(stateCounter state+1),
				stateParams=uriParameters (requestURI req)}
			newCont <- run (runDC demoStart) DS {stateDemoState=newState,stateRequest=req,stateResponse=rsp}
			return (newCont,newState)
		_ -> do
			up $ print "Nothing"
			key <- up $ newKey
			newState <- return $ makeDemoState {stateKey=key}
			newCont <- run (runDC demoStart) DS {stateDemoState=newState,stateRequest=req,stateResponse=rsp}
			return (newCont,newState)
	up $ print newState
	up $ writeState stateChannel (stateKey newState) newContState

------------------------------------------------------------------------------

demoStart :: DemoContinuation
demoStart = DC $ do
	ds <- getState 
	(state,req,rsp) <- return $ (stateDemoState ds,stateRequest ds,stateResponse ds)
	up $ up $ hPutStr stderr (showString "start" "\n")
	up $ htmlDoc $ htmlBody $ htmlForm "post" "" (do
		htmlNobrText "Page 1: Count "
		htmlNobrText (show $ stateCounter state)
		htmlBR
		htmlTextEdit "data" (lookupDict (stateParams state) "data")
		htmlBR
		htmlSubmit "Submit"
		htmlHidden "session" (stateKey state))
	up $ write rsp
	up $ write rsp
	return (Just demoNext)

demoNext :: DemoContinuation
demoNext = DC $ do
	ds <- getState
	(state,req,rsp) <- return $ (stateDemoState ds,stateRequest ds,stateResponse ds)
	up $ up $ hPutStr stderr (showString "next" "\n")
	up $ htmlDoc $ htmlBody $ htmlForm "post" "" (do
		htmlNobrText "Page 2: Count "
		htmlNobrText (show $ stateCounter state)
		htmlBR
		htmlTextEdit "data" (lookupDict (stateParams state) "data")
		htmlBR
		htmlSubmit "Submit"
		htmlHidden "session" (stateKey state))
	up $ write rsp
	up $ write rsp
	return (Just demoStart)

------------------------------------------------------------------------------

