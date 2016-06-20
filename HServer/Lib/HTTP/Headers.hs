{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- Haskell HTTP-Headers module
-- Copyright (C) 2002 Keean Schupke

module Lib.HTTP.Headers (parseHeaders,isExplorer,isMozilla,isNetscape,defaultFontSize) where
	
import Char
import Data.FiniteMap
import Lib.Monad.MonadParser
import Lib.Parser.Parser
import Lib.HTTP.Types

parseHeader :: Parser (String,String)
parseHeader = do
	a <- many1 (satisfy (\c -> c/='\r' && c/='\n' && c/=':'))
	many (satisfy (\c -> (isSpace c) || c==':'))
	b <- many (satisfy (\c -> c/='\r' && c/='\n'))
	many (satisfy (\c -> c/='\n'))
	satisfy (=='\n')
	return (a,b)

parseBlank :: Parser ()
parseBlank = do
	many (satisfy (\c -> isSpace c && c/='\n'))
	satisfy (=='\n')
	return ()

parseHeaders :: Parser [(String,String)]
parseHeaders = untilP parseBlank parseHeader

isExplorer :: HttpHeaders -> Bool
isExplorer (HttpHeaders heads) = (case lookupFM heads "User-Agent" of
	Just agent -> case parse (matchP (literal (fmap toLower item) "MSIE") (item :: Parser Char)) agent of
		Just (_,_) -> True
		Nothing -> False
	Nothing -> False) where
		
isMozilla :: HttpHeaders -> Bool
isMozilla (HttpHeaders heads) = case lookupFM heads "User-Agent" of
	Just agent -> case parse (matchP (literal (fmap toLower item) "mozilla/5.0") (item :: Parser Char)) agent of
		Just (_,_) -> True
		Nothing -> False
	Nothing -> False

isNetscape :: HttpHeaders -> Bool
isNetscape heads = case (\(HttpHeaders fm) -> lookupFM fm "User-Agent") heads of
	Just agent -> case parse (matchP (literal (fmap toLower item) "mozilla") (item :: Parser Char)) agent of
		Just (_,_) -> not $ isMozilla heads
		Nothing -> False
	Nothing -> False

defaultFontSize :: HttpHeaders -> String
defaultFontSize heads = if isExplorer heads || isMozilla heads
	then "font-size:smaller;"
	else ""

