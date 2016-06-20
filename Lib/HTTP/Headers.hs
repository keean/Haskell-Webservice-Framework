{-# LANGUAGE FlexibleContexts #-}

-- Haskell HTTP-Headers module
-- Copyright (C) 2002 Keean Schupke

module Lib.HTTP.Headers (parseHeaders,isExplorer,isMozilla,isNetscape,defaultFontSize) where
    
import Data.Char
import qualified Data.Map as Map
import Lib.Monad.MonadParser
import Lib.Parser.Parser
import Lib.HTTP.Types

parseHeader :: Parser (String,String)
parseHeader = do
    a <- many1 (satisfy (\c -> c/='\r' && c/='\n' && c/=':'))
    _ <- many (satisfy (\c -> (isSpace c) || c==':'))
    b <- many (satisfy (\c -> c/='\r' && c/='\n'))
    _ <- many (satisfy (\c -> c/='\n'))
    _ <- satisfy (=='\n')
    return (a,b)

parseBlank :: Parser ()
parseBlank = do
    _ <- many (satisfy (\c -> isSpace c && c/='\n'))
    _ <- satisfy (=='\n')
    return ()

parseHeaders :: Parser [(String,String)]
parseHeaders = untilP parseBlank parseHeader

isExplorer :: HttpHeaders -> Bool
isExplorer (HttpHeaders heads) = (case Map.lookup "User-Agent" heads of
    Just agent -> case parse (matchP (literal (fmap toLower item) "MSIE") (item :: Parser Char)) agent of
        Just (_,_) -> True
        Nothing -> False
    Nothing -> False) where
        
isMozilla :: HttpHeaders -> Bool
isMozilla (HttpHeaders heads) = case Map.lookup "User-Agent" heads of
    Just agent -> case parse (matchP (literal (fmap toLower item) "mozilla/5.0") (item :: Parser Char)) agent of
        Just (_,_) -> True
        Nothing -> False
    Nothing -> False

isNetscape :: HttpHeaders -> Bool
isNetscape heads = case (\(HttpHeaders fm) -> Map.lookup "User-Agent" fm) heads of
    Just agent -> case parse (matchP (literal (fmap toLower item) "mozilla") (item :: Parser Char)) agent of
        Just (_,_) -> not $ isMozilla heads
        Nothing -> False
    Nothing -> False

defaultFontSize :: HttpHeaders -> String
defaultFontSize heads = if isExplorer heads || isMozilla heads
    then "font-size:smaller;"
    else ""

