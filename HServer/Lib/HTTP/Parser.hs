{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

-- Parser.hs (C)2002 Keean Schupke
--
-- Reference HTTP -> Request parser
-- based on HTTP/1.1 Specification

module Lib.HTTP.Parser(httpRequest,httpResponse,httpLine,httpURI,httpURL,httpParameter) where

import Data.Char
import Data.FiniteMap
import Control.Monad
import Numeric

import Lib.Monad.MonadParser
import Lib.Parser.Parser
import Lib.HTTP.Types
import Lib.Server.Types
import Lib.Data.Version

-- HTTP ----------------------------------------------------------------------

-- httpIsOctet :: Char -> Bool
-- httpIsOctet a = case ord a of
-- 	n | n>=0 && n<256 -> True
-- 	  | otherwise -> False
	
httpIsChar :: Char -> Bool
httpIsChar a = case ord a of
		n | n>=0 && n<127 -> True
		  | otherwise -> False

-- httpIsUpAlpha :: Char -> Bool
-- httpIsUpAlpha a = case ord a of
-- 		n | n >= ord 'A' && n <= ord 'Z' -> True
-- 		  | otherwise -> False
	
-- httpIsLoAlpha :: Char -> Bool
-- httpIsLoAlpha a = case ord a of
-- 		n | n>= ord 'a' && n <= ord 'z' -> True
-- 		  | otherwise -> False

-- httpIsAlpha :: Char -> Bool
-- httpIsAlpha a = httpIsLoAlpha a || httpIsUpAlpha a
		
httpIsDigit :: Char -> Bool
httpIsDigit a = case ord a of
		n | n >= ord '0' && n <= ord '9' -> True
		  | otherwise -> False

-- httpIsCtl :: Char -> Bool
-- httpIsCtl a = case ord a of
-- 		n | (n >= 0 && n < 32) || (n == 127) -> True
-- 		  | otherwise -> False

httpIsCr :: Char -> Bool
httpIsCr a = a == '\r'

httpIsLf :: Char -> Bool
httpIsLf a = a == '\n'

httpIsSp :: Char -> Bool
httpIsSp a = a == ' '

httpIsHt :: Char -> Bool
httpIsHt a = a == '\t'

httpIsQuote :: Char -> Bool
httpIsQuote a = a=='"'

httpIsSeparator :: Char -> Bool
httpIsSeparator n
	| n == '(' = True
	| n == ')' = True
	| n == '<' = True
	| n == '>' = True
	| n == '@' = True
	| n == ',' = True
	| n == ';' = True
	| n == ':' = True
	| n == '\\' = True
	| n == '"' = True
	| n == '/' = True
	| n == '[' = True
	| n == ']' = True
	| n == '?' = True
	| n == '=' = True
	| n == '{' = True
	| n == '}' = True
	| n == ' ' = True
	| n == '\t' = True
	| otherwise = False

--------------------------------------------------------------------

httpEOL :: Parser Char
httpEOL = do
	optional $ satisfy httpIsCr
	satisfy httpIsLf
	return '\n'

httpSpace :: Parser String
httpSpace = many1 $ satisfy (\c -> httpIsSp c || httpIsHt c)

-- httpLws :: Parser Char
-- httpLws = do
-- 	httpEOL
-- 	httpSpace
-- 	return ' '

-- httpText :: Parser Char
-- httpText = satisfy (\a -> not $ httpIsCtl a) `mplus` httpLws

httpHex :: Parser Char
httpHex = satisfy $ \a -> case ord a of
	n | (n >= ord '0' && n <= ord '9') || (n >= ord 'a' && n <= ord 'f') ||
	    (n >= ord 'A' && n <= ord 'F') -> True
	  | otherwise -> False

httpToken :: Parser String
httpToken = many1 $ satisfy $ \a -> case a of
	n | ord n < 33 -> False
	  | httpIsSeparator n -> False
	  | otherwise -> True

httpLine :: Parser String
httpLine = do
	a <- many $ satisfy (\a -> not (httpIsCr a) && not (httpIsLf a))
	httpEOL
	return a

-- httpComment :: Parser String
-- httpComment = do
-- 	satisfy (== '(')
-- 	a <- optional $ (many (httpCText `mplus` httpQuotedPair)) `mplus` httpComment
-- 	case a of
-- 		Just b -> do
-- 			c <- httpComment
-- 			satisfy (== ')')
-- 			return (b `mplus` c)
-- 		Nothing -> return ""

-- httpCText :: Parser Char
-- httpCText = do
-- 	a <- httpText
-- 	case a of
-- 		'(' -> mzero
-- 		')' -> mzero
-- 		'\\' -> mzero
-- 		_ -> return a

httpQuotedString :: Parser String
httpQuotedString = do
	satisfy (== '"')
	c <- many (satisfy (\a -> not $ httpIsQuote a) `mplus` httpQuotedPair)
	satisfy (== '"')
	return c

httpQuotedPair :: Parser Char
httpQuotedPair = do
	satisfy (== '\\')
	a <- satisfy httpIsChar
	return a
	
httpVersion :: Parser HttpVersion
httpVersion = do
	literal (fmap toLower item) "http/"
	u <- many1 (satisfy httpIsDigit)
	v <- many1 subVersion
	case readDec u of
		[(w,_)] -> return $ HttpVersion (Version (w:v))
		_ -> return $ HttpVersion (Version (0:v))
	where

		subVersion :: Parser Int
		subVersion = do
			satisfy (=='.')
			a <- many1 (satisfy httpIsDigit)
			case readDec a of
				[(b,_)] -> return b
				_ -> return 0

httpURI :: URI -> Parser URI
httpURI uri = do
	a <- (httpProtocol `mplus` return (uriProtocol uri))
	b <- (httpHost `mplus` return (uriHost uri))
	c <- (httpPort `mplus` return (uriPort uri))
	d <- (httpPath `mplus` return (uriPath uri))
	e <- (do
		z <- httpParameters
		return (UriParameters $ listToFM (map (\(UriParameter u) -> u) z))) `mplus` return (uriParameters uri)
	return $ URI {
		uriProtocol=a,
		uriHost=b,
		uriPort=c,
		uriPath=d,
		uriParameters=e
	}

httpURL :: URI -> Parser URI
httpURL uri = do
	(a,b) <- ((do
		x <- httpProtocol
		literal item "//"
		y <- httpURLHost
		return (x,y)) `mplus` (do
		optional (literal item "//")
		y <- httpURLHost
		return (uriProtocol uri,y)) `mplus` return (uriProtocol uri,uriHost uri))
	c <- (httpPort `mplus` return (uriPort uri))
	d <- (httpURLPath `mplus` return (uriPath uri))
	e <- (do
		z <- httpParameters
		return (UriParameters $ listToFM (map (\(UriParameter u) -> u) z))) `mplus` return (uriParameters uri)
	return $ URI {
		uriProtocol=a,
		uriHost=b,
		uriPort=c,
		uriPath=d,
		uriParameters=e
	}

httpEscaped :: Parser Char
httpEscaped = do
	satisfy (=='%')
	a <- httpHex
	b <- httpHex
	case readHex (a:[b]) of
		((h,_):_) -> return (chr h)
		_ -> mzero

httpParameter :: Parser UriParameter
httpParameter = do
	a <- many1 (satisfy (\c -> c/='=' && not (isSpace c)))
	e <- optional $ satisfy (=='=')
	case e of
		Just _ -> do
			b <- many (satisfy (\c -> c/=';' && c/='&' && c/='%' && not (isSpace c)) `mplus` httpEscaped)
			optional $ satisfy (\c -> c==';' || c=='&')
			return $ UriParameter (a,b)
		Nothing -> return $ UriParameter (a,"")

httpParameters :: Parser [UriParameter]
httpParameters = do
	satisfy (=='?')
	ps <- many httpParameter
	return ps

httpPath :: Parser UriPath
httpPath = do
	a <- many1 (satisfy (\c -> c/='?' && not (isSpace c)))
	return $ UriPath (case a of
		('/':_) -> a
		_ -> '/':a)

httpURLPath :: Parser UriPath
httpURLPath = do
	satisfy (=='/')
	a <- many (satisfy (\c -> c/='?' && not (isSpace c)))
	return $ UriPath ('/':a)

httpPort :: Parser UriPort
httpPort = do
	satisfy (==':')
	a <- many (satisfy (\c -> c /='/' && c /='?' && not (isSpace c)))
	return $ UriPort a

httpHost :: Parser UriHost
httpHost = do
	literal item "//"
	a <- many1 (satisfy (\c -> c/='/' && c/='?' && c/=':' && not (isSpace c)))
	return $ UriHost a

httpURLHost :: Parser UriHost
httpURLHost = do
	a <- many1 (satisfy (\c -> c/='/' && c/='?' && c/=':' && not (isSpace c)))
	return $ UriHost a

httpProtocol :: Parser UriProtocol
httpProtocol = do
	a <- many (satisfy (\c -> c/='/' && c/='?' && c/=':' && not (isSpace c)))
	satisfy (==':')
	return $ UriProtocol a

httpHeader :: Parser HttpHeader
httpHeader = do
	-- a <- httpToken
	a <- many1 $ satisfy (\c -> c/='\n' && c/='\r' && c/=':')
	b <- (do
		satisfy (==':')
		httpSpace
		c <- httpLine
		return c) `mplus` (return "")
	return $ HttpHeader (a,b)
	where

	httpFieldContent :: Parser String
	httpFieldContent = do
		a <- optional $ httpToken `mplus` httpQuotedString `mplus` do
			s <- satisfy httpIsSeparator
			return [s]
		case a of
			Just b -> do
				c <- httpFieldContent
				return (b `mplus` c)
			Nothing -> return ""

httpRequest :: Connection -> Parser HttpRequest
httpRequest connection = do
	a <- httpToken
	httpSpace
	b <- httpURI $ URI {
		uriProtocol = UriProtocol "http",
		uriHost = UriHost "",
		-- uriPort = UriPort (show ((\(PortNum z) -> z) (clientLocalPort connection))),
		uriPort = UriPort $ show $ clientLocalPort connection,
		uriPath = UriPath "/",
		uriParameters = UriParameters emptyFM}
	httpSpace
	c <- httpVersion `mplus` return (HttpVersion $ Version [1,0])
	httpEOL
	d <- many httpHeader
	httpEOL
	headers <- return $ HttpHeaders $ listToFM (map (\(HttpHeader z) -> z) d)
	return $ HttpRequest {
		requestMethod = HttpMethod (map toUpper a),
		requestURI = URI {
			uriProtocol = uriProtocol b,
			uriHost = if uriHost b == UriHost ""
				then case parse hostHeader (lookupDict headers "Host") of
					Just (_,hh) -> hh
					_ -> UriHost "localhost"
				else uriHost b,
			uriPort = uriPort b,
			uriPath = uriPath b,
			uriParameters = uriParameters b},
		requestVersion = c,
		requestHeaders = headers,
		requestConnection = connection
	}
	where

	hostHeader :: Parser UriHost
	hostHeader = do
		a <- many (satisfy (\c -> c/='/' && c/='?' && c/=':' && not (isSpace c)))
		return $ UriHost a

httpResponse :: Parser HttpResponse
httpResponse = do
	version <- httpVersion
	httpSpace
	status <- httpToken
	httpSpace
	reason <- httpLine
	headers <- many httpHeader
	httpEOL
	return $ HttpResponse {
		responseVersion = version,
		responseStatus = HttpStatus $ status,
		responseReason = HttpReason $ reason,
		responseHeaders = HttpHeaders $ listToFM (map (\(HttpHeader z) -> z) headers)
	}

