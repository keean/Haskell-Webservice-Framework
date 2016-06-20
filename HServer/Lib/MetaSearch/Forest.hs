{-# OPTIONS -fglasgow-exts #-}

-- doc.hs (C)2001 Keean Schupke
--
--		main program, uses DOM module 

module Lib.MetaSearch.Forest(URL,mkForest,getDocument,getDocCook,filterForest,maybeClose,linkToURL) where

import Char
import Numeric
import IO
import Lib.MetaSearch.Parser
import Lib.MetaSearch.DOM
import Lib.MetaSearch.Filter
import Control.Exception as Exception
import Control.Concurrent
import Network
import Network.Socket 
import Foreign

type URL = String
type Protocol = String
type Location = String
type Host = String
type File = String

mkForest :: [URL] -> DOM
mkForest (u0:u') = (0,(MkElem (Document,u0,[]))):mkForest u'
mkForest [] = []

{-# INLINE fileToDocument #-}
fileToDocument :: Location -> IO (Maybe Handle,String)
fileToDocument loc = do
		h <- openFile loc ReadMode
		t <- hGetContents h
		return (Just h,t)

httpQuoteString :: String -> String
httpQuoteString [] = []
httpQuoteString (s0:s') = case s0 of
	'=' -> "%3D"++httpQuoteString s'
	'&' -> "%26"++httpQuoteString s'
	' ' -> "%20"++httpQuoteString s'
	'\t' -> "%09"++httpQuoteString s'
	'\n' -> "%0A"++httpQuoteString s'
	otherwise -> s0:httpQuoteString s'

httpArgsToString :: [Attribute] -> String
httpArgsToString [] = ""
httpArgsToString ((MkAttribute (n,v)):a') = (httpQuoteString n)++('=':(httpQuoteString v))++(case a' of
	[] -> ""
	otherwise -> '&':httpArgsToString a')

httpStdHeadersStr :: String
httpStdHeadersStr = "Content-type: application/x-www-form-urlencoded\r\n" ++
	"User-Agent: Mozilla/5.0 (X11; U; SunOS sun4u; en-US; rv:1.0.1) Gecko/20020918\r\n" ++
	"Keep-Alive: 0\r\n" ++
	"Connection: close\r\n"

httpGetHeadersStr :: String
httpGetHeadersStr = "Content-type: application/x-www-form-urlencoded\r\n" ++
	"User-Agent: Mozilla/5.0 (X11; U; SunOS sun4u; en-US; rv:1.0.1) Gecko/20020918\r\n" ++
	"Keep-Alive: 0\r\n" ++
	"Connection: close\r\n"

httpPostHeadersStr :: String
httpPostHeadersStr = "Content-type: multipart/form-data, boundary=XXXXXXXX\r\n" ++
	"User-Agent: Mozilla/5.0 (X11; U; SunOS sun4u; en-US; rv:1.0.1) Gecko/20020918\r\n" ++
	"Keep-Alive: 0\r\n" ++
	"Connection: close\r\n"

httpGetHeaders' :: [Attribute] -> String
httpGetHeaders' [] = httpGetHeadersStr
httpGetHeaders' (MkAttribute (n,v):[]) = n++"="++v++"\r\n"++httpGetHeadersStr
httpGetHeaders' (MkAttribute (n,v):cs@(_:_)) = n++"="++v++"; "++(httpGetHeaders' cs)

httpGetHeaders :: [Attribute] -> String
httpGetHeaders [] = httpGetHeadersStr
httpGetHeaders h = "Cookie: " ++ httpGetHeaders' h

httpPostHeaders' :: [Attribute] -> String
httpPostHeaders' [] = httpPostHeadersStr
httpPostHeaders' (MkAttribute (n,v):[]) = n++"="++v++"\r\n"++httpPostHeadersStr
httpPostHeaders' (MkAttribute (n,v):cs@(_:_)) = n++"="++v++"; "++(httpPostHeaders' cs)

httpPostHeaders :: [Attribute] -> String
httpPostHeaders [] = httpPostHeadersStr
httpPostheaders h = "Cookie: " ++ httpPostHeaders' h

httpStdHeaders' :: [Attribute] -> String
httpStdHeaders' [] = httpStdHeadersStr
httpStdHeaders' (MkAttribute (n,v):[]) = n++"="++v++"\r\n"++httpStdHeadersStr
httpStdHeaders' (MkAttribute (n,v):cs@(_:_)) = n++"="++v++"; "++(httpStdHeaders' cs)

httpStdHeaders :: [Attribute] -> String
httpStdHeaders [] = httpStdHeadersStr
httpStdHeaders h = "Cookie: " ++ httpStdHeaders' h

httpArgsToPost :: [Attribute] -> String
httpArgsToPost [] = "--XXXXXXXX\r\n"
httpArgsToPost ((MkAttribute (n,v)):a') = "--XXXXXXXX\r\ncontent-disposition: form-data; name=\""
	++n++"\"\r\n\r\n"++v++"\r\n"++httpArgsToPost a'

portNum :: PortID -> Int
portNum (PortNumber p) = fromIntegral p
portNum _ = 0

{-# INLINE httpPostToDocument #-}
httpPostToDocument :: String -> Location -> [Attribute] -> IO (Maybe Handle,String)
httpPostToDocument headers loc args = case splitLocation loc of
	(host,port,file) -> do
		 skt <- connectTo host port
		 (do
			 case httpArgsToString args of
				ps -> do
					hPutStr skt ("POST " ++ file ++ " HTTP/1.0\r\n"
						++ "Host: "++host++":"++(show (portNum port))++"\r\n"
						++ headers 
						++ "Content-Length: "++(showInt (length ps) "\r\n\r\n")
						++ ps ++ "\r\n")
					-- hPutStr stderr ("POST " ++ file ++ " HTTP/1.0\r\n"
					-- 	++ headers 
					-- 	++ "Content-Length: "++(showInt (length ps) "\r\n\r\n")
					-- 	++ ps ++ "\r\n")
			 hFlush skt
			 t <- hGetContents skt
			 return (Just skt,t)) `Exception.catch` (\e -> do hClose skt;throw e)

{-# INLINE httpGetToDocument #-}
httpGetToDocument :: String -> Location -> [Attribute] -> IO (Maybe Handle,String)
httpGetToDocument headers loc args = case splitLocation loc of
	(host,port,file) -> do
		skt <- connectTo host port
		-- hPutStr stderr $ shows skt "\n"
		-- hSetBuffering skt (BlockBuffering $ Just 1500)
		(do
			case httpArgsToString args of
	 			[] -> do
					hPutStr skt ("GET "++file++" HTTP/1.0\r\n"
						++ "Host: "++host++":"++(show (portNum port))++"\r\n"
						++ headers ++ "\r\n")
					-- hPutStr stderr ("{GET "++file++" HTTP/1.0\r\n"
					-- 	++ "Host: "++host++":"++(show (portNum port))++"\r\n"
					-- 	++ headers ++ "}\r\n")
				ps -> do
					hPutStr skt ("GET "++file++('?':ps)++" HTTP/1.0\r\n"
						++ "Host: "++host++":"++(show (portNum port))++"\r\n"
						++ headers ++ "\r\n")
					-- hPutStr stderr ("{GET "++file++('?':ps)++" HTTP/1.0\r\n"
					-- 	++ "Host: "++host++":"++(show (portNum port))++"\r\n"
					-- 	++ headers ++ "}\r\n")
			hFlush skt
	 		t <- hGetContents skt
			return (Just skt,t)) `Exception.catch` (\e -> do hClose skt;throw e)

showDocument :: Location -> [Attribute] -> IO (Maybe Handle,String)
showDocument loc args = case splitLocation loc of
	(host,port,file) -> do
		case httpArgsToString args of
			[] -> hPutStr stderr ("GET "++file++" HTTP/1.0\n\n")
			ps -> hPutStr stderr ("GET "++file++('?':ps)++" HTTP/1.0\n\n")
		return (Nothing,[])

getDocument :: Elem -> IO (Maybe Handle,DOM)
getDocument e = getDocument' [] 5 e where

getDocCook :: Elem -> [Attribute] -> IO (Maybe Handle,DOM)
getDocCook e cookies = getDocument' cookies 1 e 

{-# INLINE maybeClose #-}
maybeClose :: Maybe Handle -> IO ()
maybeClose (Just h) = (hClose h) `Exception.catch` (\e -> hPutStr stderr $ shows e "\n")
maybeClose _ = return ()

{-# INLINE getDocument' #-}
getDocument' :: [Attribute] -> Int -> Elem -> IO (Maybe Handle,DOM)
getDocument' cookies i e = case e of
	(MkElem (Document,url,a)) -> (do
		-- print url
			(mh,d) <- case splitURL url of
				("FILE",l) -> fileToDocument l
				("HTTP",l) -> httpGetToDocument (httpStdHeaders cookies) l a
				("HTTP-POST",l) -> httpPostToDocument (httpGetHeaders cookies) l a
				-- ("HTTP-POST",l) -> httpGetToDocument (httpGetHeaders cookies) l a
				("HTTP-GET",l) -> httpGetToDocument (httpGetHeaders cookies) l a
				("SHOW",l) -> showDocument l a
				otherwise -> return (Nothing,[])
			-- hPutStr stderr $ showString d "\n"
			return $ (mh,createDOM d))
		`Exception.catch` (\x -> do
			hPutStr stderr ((shows x . showString " [" . shows i) "s ...]\n")
			threadDelay (i*500000)
			getDocument' cookies (i*2) e)
	otherwise -> return (Nothing,[])

{-# INLINE getProtocol #-}
getProtocol :: Parser Protocol
getProtocol = do
	a <- (required . untilChar) (==':')
	whileChar (==':')
	return a

{-# INLINE getSocket #-}
getSocket :: Parser Host
getSocket = do
	satisfy (=='/')
	(required . whileChar) (=='/')
	a <- untilChar (=='/')
	return a

{-# INLINE getHost #-}
getHost :: Parser Host
getHost = do
	a <- (required . untilChar) (==':')
	(required . whileChar) (==':')
	return a

{-# INLINE splitLocation #-}
splitLocation :: Location -> (Host,PortID,File)
splitLocation loc = case parse getSocket loc of
	Consumed (Ok h f) -> case parse getHost h of
		Consumed (Ok h' p) -> (h',PortNumber ((fromIntegral . read) p),f)
		otherwise -> (h,PortNumber (fromIntegral 80),f)
	otherwise -> ("localhost",PortNumber (fromIntegral 80),loc)

{-# INLINE splitURL #-}
splitURL :: URL -> (Protocol,Location)
splitURL url = case parse getProtocol url of
	Consumed (Ok p l) -> (map toUpper p,l)
	otherwise -> ("NULL",url)

filterForest :: FilterDOM DOM -> DOM -> IO DOM
filterForest f d = case d of
	(ie@(i,e@(MkElem (Document,_,_))):d') -> do
		(h,a) <- getDocument e
		case filterDOM f a of
			DOMOut a' _ -> do
				a'' <- filterForest f (moveRoot i a')
				d'' <- filterForest f d' 
				return (a''++d'')
			otherwise -> do
				d'' <- filterForest f d'
				return d''
	(ie:d') -> do
		d'' <- filterForest f d'
		return (ie:d'')
	otherwise -> return []
	
linkToURL :: String -> String -> String -> URL
linkToURL site m link = case parseResult (stringNcs "http://") link of
	Ok _ l -> if m /= "" then "http-"++m++"://"++l else "http://"++l
	_ -> case parseResult (stringNcs "//") link of
		Ok _ l -> if m /= "" then "http-"++m++"://"++l else "http://"++l
		_ -> case parseResult (stringNcs "/") link of
			Ok _ l -> if m /= "" then "http-"++m++"://"++site++"/"++l else "http://"++site++"/"++l
			_ -> if m /= "" then "http-"++m++"://"++site++"/"++link else "http://"++site++"/"++link

