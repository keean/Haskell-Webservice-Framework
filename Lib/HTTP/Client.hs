module Lib.HTTP.Client (httpClient,ServerConnection(..),httpGetPage,httpGetPageString,after,first) where

import System.IO
import Data.Char
import Network
import Numeric
import Foreign
--import Control.Exception

import Lib.Monad.MonadIO
import Lib.Monad.MonadParser
import Lib.Parser.Parser
import Lib.XML.Types
import Lib.HTML.DOM
import Lib.HTTP.Types
import Lib.HTTP.Parser
import Lib.Data.Version 

data ServerConnection = ServerConnection {
    serverHostname :: HostName,
    serverPort :: PortID,
    serverHandle :: Handle
} deriving Show

{-
instance Show PortID where
    showsPrec _ (PortNumber i) = shows i        
    showsPrec _ _ = showString "Bad port-number\n"
-}

debugConnectTo :: MonadIO m => String -> PortID -> m Handle
debugConnectTo host port = do
    ioHPutStr stderr ((showString "connectTo " . shows host . showChar ' ' . shows port) "\n")
    h <- ioConnectTo host port
    ioHSetBuffering h (BlockBuffering Nothing)
    -- ioHSetBuffering h LineBuffering
    -- ioHSetBuffering h NoBuffering
    return h

uriToPortID :: URI -> PortID
uriToPortID uri = PortNumber $ fromIntegral $ ((read ((\(UriPort p) -> p) (uriPort uri))) :: Word16)

connectToURI :: MonadIO m => URI -> m ServerConnection
connectToURI uri = buildConnection ((\(UriHost h) -> h) (uriHost uri)) (uriToPortID uri)

buildConnection :: MonadIO m => String -> PortID -> m ServerConnection
buildConnection host port = do
    h <- debugConnectTo host port
    return $ ServerConnection {
        serverHostname = host,
        serverPort = port,
        serverHandle = h
    }

httpClient :: MonadIO m => HttpRequest -> m ServerConnection
httpClient req = connectToURI (requestURI req)

httpGetPageString :: MonadIO m => HttpRequest -> String -> m (Maybe (HttpResponse,DOM))
httpGetPageString req uriString = case parse (httpURI defaultURI) uriString of
    Nothing -> return Nothing
    Just (_,uri) -> httpGetPage req uri

httpGetPage :: MonadIO m => HttpRequest -> URI -> m (Maybe (HttpResponse,DOM))
httpGetPage req uri = (do
    server <- httpClient thisRequest
    ((getPage server) `ioCatch` (\e -> do
        ioHClose (serverHandle server)
        ioThrow e))) `ioCatch` (\e -> do
            ioHPutStr stderr $ shows e "\n"
            return Nothing)
    where

        getPage :: MonadIO m => ServerConnection -> m (Maybe (HttpResponse,DOM))
        getPage server = do
            contents <- ioHGetContents (serverHandle server)
            ioHPutStr (serverHandle server) (show thisRequest)
            ioHPutStr stderr (show thisRequest)
            ioHFlush (serverHandle server)
            -- hPutStr stderr "RAW:\n"
            -- hPutStr stderr contents
            -- hPutStr stderr "\n"
            rsp <- case parse httpResponse contents of
                Just (next,response) -> do
                    rsp <- return $ case responseStatus response of
                        (HttpStatus s) | s=="301" || s=="302" -> addToDict response "Location" $ case parse (httpURI uri)
                            (lookupDict response "Location") of
                                Just (_,u) -> show $ addToDict (requestURI req) "uri" (show u)
                                _ -> show $ requestURI req
                        _ -> response
                    ioHPutStr stderr (show rsp)
                    te <- return $ lookupDict rsp "Transfer-Encoding"
                    ioHPutStr stderr $ (showString te) "\n"
                    case parse (literal (fmap toLower item) "chunked") te of
                        Just _ -> case parse httpLine next of
                            Just (n2,l) -> case readHex l of
                                ((len,_):_) -> do
                                    ioHPutStr stderr $ (showString "Chunk: " . showInt len) "\n"
                                    return $! Just (rsp,htmlToDOM (getChunk n2 len ""))
                                _ -> do
                                    ioHPutStr stderr "Chunk: Failed to convert.\n"
                                    return $! Just (rsp,htmlToDOM n2)
                            _ -> do
                                ioHPutStr stderr "Chunk: Read chunk size failed.\n"
                                return $! Just (rsp,htmlToDOM next)
                        _ -> case readDec (lookupDict rsp "Content-Length") of
                            ((len,_):_) -> do
                                ioHPutStr stderr $ (showString "Content-Length: " . showInt len) "\n"
                                a <- return $ first next len
                                -- hPutStr stderr $ showString a "\n"
                                return $! Just (rsp,htmlToDOM a)
                            _ -> do
                                ioHPutStr stderr "Content-Length: Read failed.\n"
                                return $! Just (rsp,htmlToDOM next)
                _ -> do
                    ioHPutStr stderr "Failed to parse response.\n"
                    return $! Nothing
            return rsp

        thisRequest :: HttpRequest
        thisRequest = HttpRequest {
            requestMethod = HttpMethod "GET",
            requestURI = uri, -- makeURI {uriPath=(uriPath uri),uriParameters=(uriParameters uri)},
            requestVersion = HttpVersion (Version [1,1]),
            requestHeaders = addListToDict (requestHeaders req) [
                ("Host",(showUriHost (uriHost uri) . showChar ':' . showUriPort (uriPort uri)) ""),
                ("Accept-Encoding",""),
                ("Accept","text/html"),
                ("Connection","close")
            ],
            requestConnection = undefined}

        getChunk :: String -> Int -> ShowS
        getChunk s i
            | i<=0 = case parse httpLine s of
                Just (_,_) -> id
                _ -> id
            | otherwise = case parse httpLine (after s i) of
                Just (next,l1) -> case readHex l1 of
                    ((len,_):_) -> f . getChunk next len
                    _ -> case parse httpLine next of
                        Just (n2,l2) -> case readHex l2 of
                            ((len,_):_) -> f . getChunk n2 len
                            _ -> f
                        _ -> f
                _ -> f
            where
            
                f :: ShowS
                f = showString (first s i)
                
first :: String -> Int -> String
first str i
    | i>0 = case str of
        (c0:cs) -> c0:first cs (i-1) 
        [] -> []
    | otherwise = []

after :: String -> Int -> String
after str i
    | i>0 = case str of
        (_:cs) -> after cs (i-1)
        [] -> []
    | otherwise = str

