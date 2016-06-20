{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
-- Haskell HTML/SQL module
-- Copyright (C) 2002 Keean Schupke

module Modules.Admin.App (initialise) where

import IO
import Monad
import FiniteMap

import Data.FiniteMap

import Lib.Monad.MonadIO
import Lib.Server.Types
import Lib.XML.Types
import Lib.HTML.MonadHtml
import Lib.HTML.HtmlFragmentT
import Lib.HTTP.Types

initialise :: IO Application
initialise = do
	hPutStr stderr "initialise admin...\n"
	return ("/admin",AppMethods {start=return (),stop=return (),request=handle})

handle :: RequestHandler
handle req response = do
	_ <- case lookupFM ((\(UriParameters a) -> a) (uriParameters $ requestURI req)) "RESTART" of
		Just "on" -> fail "server restarted..."
		_ -> return ()
	htmlText "HTTP/1.0 200 OK\n"
	htmlHeaders [MkAttribute ("Set-Cookie","test=1; path=/;")]
	htmlText "\n"
	htmlHead $ htmlTitle "HyperServer/Admin"
	htmlBody $ do
		htmlH1 $ htmlText "HyperServer Admin Page"
		write response
		-- ioThreadDelay 3000000
		htmlForm $ do
			htmlCheckbox "RESTART"
			htmlText "Restart Server"
			htmlSubmit "Submit"
		htmlSmall (showHeaders $ fmToList $ ((\(HttpHeaders a) -> a) (requestHeaders req)))
	write response

showHeaders :: (MonadIO m,MonadHtmlBase m,MonadHtml m) => [(String,String)] -> m ()
showHeaders [] = htmlNull
showHeaders ((n,v):hs) = do
	htmlB $ htmlText n
	htmlText ": "
	htmlText v
	htmlBR
	showHeaders hs

