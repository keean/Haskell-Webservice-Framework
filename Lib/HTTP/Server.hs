-- server.hs: Copyright (C)2002 Keean Schupke
--
--      HyperServer main module

module Lib.HTTP.Server (httpHandler,respondRaw,respondHTML,respondRedirect,htmlNotFound) where

--import GHC.IO
import Data.Char
import System.IO
import qualified Data.Map as Map
--import Data.Set
--import Control.Monad
import Numeric

import Lib.Data.Version
import Lib.Arrow.Runnable
import Lib.Monad.MonadIO
import Lib.Monad.MonadParser
--import Lib.Monad.MonadT
import Lib.Parser.Parser
import Lib.XML.Generator
import Lib.HTML.MonadHtml
import Lib.HTML.HtmlFragmentT
--import Lib.HTML.Types
import Lib.HTTP.Types
import Lib.HTTP.Parser
import Lib.HTTP.Client
-- import Lib.HTTP.Client
import Lib.Server.Types

ioReply :: MonadIO m => Handle -> HtmlFragmentT m ()
ioReply h = do
    sd <- htmlPop
    doc <- return $ showDOM (sd []) "\n"
    ioHPutStr h doc
    ioHFlush h

ioChunked :: MonadIO m => Handle -> HtmlFragmentT m ()
ioChunked h = do
    sd <- htmlPop
    doc <- return $ showDOM (sd []) ""
    ioHPutStr h ((showIntAtBase 16 intToDigit $ length doc) "\r\n")
    ioHPutStr h doc
    ioHPutStr h "\r\n"
    ioHFlush h

-- lookupAttribute :: [XmlAttribute] -> String -> String
-- lookupAttribute [] _ = ""
-- lookupAttribute (XmlAttribute (n,v):as) s = if (map toLower s)==(map toLower n)
--  then valueToString v ""
--  else lookupAttribute as s
--  where

--  valueToString :: [XmlElement] -> ShowS
--  valueToString [] = id
--  valueToString (x0:xs) = (case x0 of
--      (CharData t) -> showString t
--      (EntityRef e) -> showChar '&' . showString e . showChar ';'
--      _ -> id) . valueToString xs

-- addAttribute :: (String,String) -> [XmlAttribute] -> [XmlAttribute]
-- addAttribute (n,v) [] = [XmlAttribute (n,[CharData v])]
-- addAttribute s@(n,v) (a0@(XmlAttribute (n0,_)):as) = if (map toLower n)==(map toLower n0)
--  then XmlAttribute (n,[CharData v]):as
--  else a0:addAttribute s as

respondHTML :: HttpRequest -> (Response -> IO ()) -> HttpRspStatus -> IO ()
respondHTML req body rs = case requestVersion req of
    HttpVersion v
        | v >= Version [1,1] -> do
            hPutStr (clientHandle $ requestConnection req) (show $ HttpResponse {
                responseVersion = HttpVersion $ Version [1,1],
                responseStatus = httpRspToStatus rs,
                responseReason = httpRspToReason rs,
                responseHeaders = addListToDict (HttpHeaders Map.empty) [
                    ("Transfer-Encoding","chunked"),
                    ("Connection",lookupWithDefaultDict req "Keep-Alive" "Connection"),
                    ("Content-Type","text/html")]})
            hFlush (clientHandle $ requestConnection req)
            body (Response {write=(ioChunked $ clientHandle (requestConnection req))})
        | otherwise -> do
            hPutStr (clientHandle $ requestConnection req) (show $ HttpResponse {
                responseVersion = HttpVersion $ Version [1,0],
                responseStatus = httpRspToStatus rs,
                responseReason = httpRspToReason rs,
                responseHeaders = addToDict (HttpHeaders Map.empty) "Content-Type" "text/html"})
            hFlush (clientHandle $ requestConnection req)
            body (Response {write=(ioReply $ clientHandle (requestConnection req))})

respondRaw :: HttpRequest -> String -> HttpRspStatus -> IO ()
respondRaw req body rs = do
    hPutStr (clientHandle $ requestConnection req) (show $ HttpResponse {
        responseVersion = requestVersion req,
        responseStatus = httpRspToStatus rs,
        responseReason = httpRspToReason rs,
        responseHeaders = addListToDict (HttpHeaders Map.empty) [
            ("Content-Length",showInt (length body) ""),
            ("Content-Type","text/csv")]})
    hPutStr (clientHandle $ requestConnection req) body
    hFlush (clientHandle $ requestConnection req)

respondRedirect :: HttpRequest -> String -> IO ()
respondRedirect req url = do
    hPutStr (clientHandle $ requestConnection req) (show $ HttpResponse {
        responseVersion = requestVersion req,
        responseStatus = HttpStatus "302",
        responseReason = HttpReason "Redirect",
        responseHeaders = addListToDict (HttpHeaders Map.empty) [
            ("Content-Length","0"),
            ("Content-Type","text/html"),
            ("Location",url)]})
    hFlush (clientHandle $ requestConnection req)

getPostData :: HttpRequest -> String -> IO (HttpRequest,String)
getPostData req contents = case readDec $ lookupDict req "Content-Length" of
    ((len,_):_) -> case parse (many httpParameter) (first contents len) of
        Just (_,params) -> return (req {requestURI=addListToDict (requestURI req) (map (\(UriParameter u) -> u) params)},
            after contents len)
        Nothing -> return (req,contents)
    _ -> return (req,contents)

httpHandler :: HttpHandler -> ConnectionHandler
httpHandler handler clientConnection cont = case parse (httpRequest clientConnection) cont of
    Nothing -> case parse httpLine cont of
        Just (nxt,_) -> httpHandler handler clientConnection nxt
        Nothing -> do
            hPutStr stderr "localHandler: no more requests\n"
            return ()
    Just (initialContents,initialReq) -> do
        -- hPutStr stderr $ shows initialReq "\n"
        (req,contents) <- getPostData initialReq initialContents
        case requestMethod req of
            HttpMethod m | m=="GET" || m=="POST" -> handler req clientConnection
            _ -> respondHTML req (\rsp -> run (htmlNotImplemented req rsp)) HttpNotImplemented
        if requestVersion req >= HttpVersion (Version [1,1])
            then do
                case parse (literal (fmap toLower item) "close") (lookupDict req "Connection") of
                    Just _ -> return ()
                    _ -> httpHandler handler (requestConnection req) contents
            else do
                case parse (literal (fmap toLower item) "keep-alive") (lookupDict req "Connection") of
                    Just _ -> httpHandler handler (requestConnection req) contents
                    _ -> return ()

htmlNotFound :: HttpRequest -> Response -> HtmlFragmentT IO ()
htmlNotFound req rsp = do
    htmlDoc $ htmlBody $ do
        htmlNobrText "HyperServer (C)2002,2003 Fry-IT Ltd."
        htmlBR
        htmlNobrText "Error 404, Not Found: "
        htmlNobrText $ show $ requestURI req
        htmlBR
    write rsp
    write rsp

htmlNotImplemented :: HttpRequest -> Response -> HtmlFragmentT IO ()
htmlNotImplemented req rsp = do
    htmlDoc $ htmlBody $ do
        htmlNobrText "HyperServer (C)2002,2003 Fry-IT Ltd."
        htmlBR
        htmlNobrText "Error 501, Not Implemented: "
        htmlNobrText $ show $ (requestMethod req)
        htmlBR
    write rsp
    write rsp
