{-# OPTIONS -fglasgow-exts #-}
-- Haskell SQL module
-- Copyright (C) 2002 Keean Schupke

module Lib.HTTP.Types (URI(..),UriParameter(..),HttpHeaders(..),
	UriProtocol(..),UriHost(..),UriPort(..),UriPath(..),UriParameters(..),
	HttpMethod(..),HttpVersion(..),HttpHeader(..),HttpBody(..),
	HttpStatus(..),HttpRequest(..),
	HttpResponse(..),HttpReason(..),
	Response(..),Dictionary(..),defaultURI,HttpRspStatus(..),httpRspToStatus,httpRspToReason,
	HttpHandler,showUriProtocol,showUriHost,showUriPort,showUriPath,showUriParameters,makeURI) where

import Data.FiniteMap
import IO
import Lib.Server.Types
import Lib.HTML.Types
import Lib.Data.Version
import Lib.HTTP.Templates

$(stringType "UriProtocol")
$(stringType "UriHost")
$(stringType "UriPort")
$(stringType "UriPath")

$(stringType "HttpMethod")
$(stringType "HttpBody")
$(stringType "HttpStatus")
$(stringType "HttpReason")

newtype UriParameter = UriParameter (String,String)
newtype HttpHeaders = HttpHeaders (FiniteMap String String)
newtype UriParameters = UriParameters (FiniteMap String String)

showUriParameter :: UriParameter -> ShowS
showUriParameter (UriParameter (n,v)) = showString n . showChar '=' . showString v

showUriParameterList :: [UriParameter] -> ShowS
showUriParameterList [] = id
showUriParameterList (p0:ps) = showUriParameter p0 . case ps of
	[] -> id
	_ -> showChar ';' . showUriParameterList ps

instance Show UriParameter where
	showsPrec _ p = showUriParameter p
	showList ps = showUriParameterList ps

showUriParameters :: UriParameters -> ShowS
showUriParameters (UriParameters p) = case fmToList p of
	[] -> id
	ps -> showChar '?' . showUriParameterList (fmap UriParameter ps)

instance Show UriParameters where
	showsPrec _ p = showUriParameters p

data URI = URI {
	uriProtocol :: UriProtocol,
	uriHost :: UriHost,
	uriPort :: UriPort,
	uriPath :: UriPath,
	uriParameters :: UriParameters
}

makeURI :: URI
makeURI = URI {
	uriProtocol = UriProtocol "",
	uriHost = UriHost "",
	uriPort = UriPort "",
	uriPath = UriPath "",
	uriParameters = UriParameters emptyFM
}

defaultURI :: URI
defaultURI = URI {
	uriProtocol = UriProtocol "http",
	uriHost = UriHost "localhost",
	uriPort = UriPort "80",
	uriPath = UriPath "/",
	uriParameters = UriParameters emptyFM
}

showURI :: URI -> ShowS
showURI uri = showUriProtocol (uriProtocol uri) . showString "://" . showConnectionURI uri . showLocalURI uri

showConnectionURI :: URI -> ShowS
showConnectionURI uri = showUriHost (uriHost uri) . showChar ':' . showUriPort (uriPort uri)

showLocalURI :: URI -> ShowS
showLocalURI uri = case uriPath uri of
		UriPath (p:_) | p /= '/' -> showChar '/'
		_ -> id
	. showUriPath (uriPath uri)
	. showUriParameters (uriParameters uri)

instance Show URI where
	showsPrec _ uri = showURI uri

newtype HttpVersion = HttpVersion Version deriving (Eq,Ord)
newtype HttpHeader = HttpHeader (String,String)

data HttpRequest = HttpRequest {
	requestMethod :: HttpMethod,
	requestURI :: URI,
	requestVersion :: HttpVersion,
	requestHeaders :: HttpHeaders,
	requestConnection :: Connection
}

data HttpResponse = HttpResponse {
	responseVersion :: HttpVersion,
	responseStatus :: HttpStatus,
	responseReason :: HttpReason,
	responseHeaders :: HttpHeaders
}

type HttpHandler = HttpRequest -> Connection -> IO ()

data HttpRspStatus = HttpOk | HttpNotFound | HttpNotImplemented | HttpUnknown

httpRspToStatus :: HttpRspStatus -> HttpStatus
httpRspToStatus HttpOk = HttpStatus "200"
httpRspToStatus HttpNotFound = HttpStatus "404"
httpRspToStatus HttpNotImplemented = HttpStatus "501"
httpRspToStatus HttpUnknown = HttpStatus "000"

httpRspToReason :: HttpRspStatus -> HttpReason
httpRspToReason HttpOk = HttpReason "OK"
httpRspToReason HttpNotFound = HttpReason "NotFound"
httpRspToReason HttpNotImplemented = HttpReason "NotImplemented"
httpRspToReason HttpUnknown = HttpReason "UNKNOWN"

instance Show HttpVersion where
	showsPrec _ (HttpVersion v) = showString "HTTP/" . shows v

instance Show HttpHeaders where
	showsPrec _ (HttpHeaders h) = showHeaders (fmToList h)

instance Show HttpHeader where
	showsPrec _ (HttpHeader (n,v)) = showString n . showString ": " . showString v . showString "\r\n"
	showList [] = id
	showList (h0:hs) = shows h0 . shows hs

showHeaders :: [(String,String)] -> ShowS
showHeaders [] = id
showHeaders ((n,v):hs) = showString n . showString ": " . showString v . showString "\r\n"
	. showHeaders hs

instance Show HttpRequest where
	showsPrec _ req = shows (requestMethod req) . showChar ' ' . showLocalURI (requestURI req) .
		showChar ' ' . shows (requestVersion req) . showString "\r\n" .
		shows (addToDict (requestHeaders req) "Host" (showConnectionURI (requestURI req) "")) . showString "\r\n"

instance Show HttpResponse where
	showsPrec _ rsp = shows (responseVersion rsp) . showChar ' ' . shows (responseStatus rsp)
		. showChar ' ' . shows (responseReason rsp) . showString "\r\n"
		. shows (responseHeaders rsp) . showString "\r\n"

data Response = Response {
	write :: HtmlFragmentT IO ()
}

------------------------------------------------------------------------------

class Dictionary c where
	toDictionary :: c -> FiniteMap String String
	fromDictionary :: c -> FiniteMap String String -> c
	lookupDict :: c -> String -> String
	lookupWithDefaultDict :: c -> String -> String -> String
	existsDict :: c -> String -> Bool
	emptyDict :: c -> c
	addToDict :: c -> String -> String -> c
	addListToDict :: c -> [(String,String)] -> c
	delFromDict :: c -> String -> c
	delListFromDict :: c -> [String] -> c
	listToDict :: c -> [(String,String)] -> c

	lookupDict d x = lookupWithDefaultFM (toDictionary d) "" x 
	lookupWithDefaultDict d y x = lookupWithDefaultFM (toDictionary d) y x
	existsDict d x = elemFM x (toDictionary d)
	emptyDict d = fromDictionary d emptyFM
	addToDict d n v = fromDictionary d (addToFM (toDictionary d) n v)
	addListToDict d x = fromDictionary d (addListToFM (toDictionary d) x)
	delFromDict d x = fromDictionary d (delFromFM (toDictionary d) x)
	delListFromDict d x = fromDictionary d (delListFromFM (toDictionary d) x)
	listToDict d x = fromDictionary d (listToFM x)

instance Dictionary HttpHeaders where
	toDictionary headers = (\(HttpHeaders z) -> z) $ headers
	fromDictionary _ dict = HttpHeaders $ dict

instance Dictionary HttpRequest where
	toDictionary req = (\(HttpHeaders z) -> z) (requestHeaders req)
	fromDictionary req dict = HttpRequest {
		requestMethod = requestMethod req,
		requestURI = requestURI req,
		requestVersion = requestVersion req,
		requestHeaders = HttpHeaders dict,
		requestConnection = requestConnection req
	}

instance Dictionary HttpResponse where
	toDictionary rsp = (\(HttpHeaders z) -> z) (responseHeaders rsp)
	fromDictionary rsp dict = HttpResponse {
		responseVersion = responseVersion rsp,
		responseStatus = responseStatus rsp,
		responseReason = responseReason rsp,
		responseHeaders = HttpHeaders dict
	}

instance Dictionary URI where
	toDictionary uri = (\(UriParameters z) -> z) (uriParameters uri)
	fromDictionary uri dict = URI {
		uriProtocol = uriProtocol uri,
		uriHost = uriHost uri,
		uriPort = uriPort uri,
		uriPath = uriPath uri,
		uriParameters = UriParameters dict
	}

instance Dictionary UriParameters where
	toDictionary (UriParameters fm) = fm
	fromDictionary _ dict = UriParameters dict

