{-# LANGUAGE ScopedTypeVariables #-}

-- redirect.hs (C)2001 Keean Schupke
--
--      Follow Redirects!

module Lib.MetaSearch.Redirect(getRedirect,followRedirects,getPageWithRedirects) where
import Data.Char
import System.IO
import Lib.MetaSearch.Parser
import Lib.MetaSearch.DOM
--import Lib.MetaSearch.Filter
import Lib.MetaSearch.Forest
--import Lib.MetaSearch.Forms
import Lib.MetaSearch.Cookies
import Control.Exception as Exception

type FormVars = [Attribute]

httpEquiv :: [Attribute] -> String
httpEquiv as =(map toLower (getAttributeAsString "http-equiv" as))

redirectParser :: Parser String
redirectParser = do
    _ <- untilChar (==';')
    _ <- whileChar (==';')
    _ <- untilChar (=='=')
    whileChar (=='=')

content :: [Attribute] -> String
content as = case parseResult redirectParser (getAttributeAsString "content" as) of
    Ok _ url -> url
    _ -> ""

getMeta :: DOM -> String
getMeta [] = ""
getMeta ((_,MkElem (Tag,"META",a)):d')
    | httpEquiv a == "refresh" = content a
    | otherwise = getMeta d'
getMeta (_:d') = getMeta d'

redirect :: [Attribute] -> String
redirect [] = ""
redirect (MkAttribute (n,v):as) = if (map toLower n) == "location"
    then v
    else redirect as

getRedirect :: DOM -> String
getRedirect [] = ""
getRedirect ((_, MkElem (elm, _, a)) : _) 
    | elm == Header = let rda = redirect a in if rda /= "" then rda else ""
    | otherwise = ""

getEither :: DOM -> String
getEither d = case getRedirect d of
    srd | srd /= "" -> srd
         | otherwise -> case getMeta d of
            mrd | mrd /= "" -> mrd
                 | otherwise -> ""

mClose :: Maybe Handle -> IO ()
mClose (Just h) = (hClose h) `Exception.catch` (\(e :: SomeException) -> hPutStr stderr $ shows e "\n")
mClose _ = return ()

maybeAddSite :: String -> String -> String
maybeAddSite s r =
    let t = case (parseResult ((required . untilChar) (==':')) s) of
            Ok a _ -> a++"://"
            _ -> "http://"
        in case (parseResult (stringNcs "http://") r) of
            Ok _ r' -> (t++r')
            _ -> case (parseResult (stringNcs "//") r) of
                Ok _ r' -> (t++r')
                _ -> (s++r)

followRedirects :: String -> (Maybe Handle,DOM) -> Cookies -> IO (Maybe Handle,DOM)
followRedirects site md@(mh,d) cookies = case getEither d of
    r | r /= "" -> do
            mClose mh
            src <- getDocCook (MkElem (Document,maybeAddSite site r,[])) cookies
            rec <- followRedirects site src cookies
            return rec
      | otherwise -> return md

getPageWithRedirects :: Cookies -> FormVars -> URL -> IO (URL,Maybe Handle,DOM)
getPageWithRedirects cookies vars url = getPageWithRedirects' url (url,cookies,vars)

getPageWithRedirects' :: URL -> (URL,Cookies,FormVars) -> IO (URL,Maybe Handle,DOM)
getPageWithRedirects' site (url,cookies,vars) = do
    (h0,src) <- getDocCook (MkElem (Document,maybeAddSite site url,vars)) cookies
    case getEither src of
        r   | r /= "" -> do
                mClose h0
                uhd <- getPageWithRedirects' site (r,vars,cookies)
                return uhd
            | otherwise -> do
                return (url,h0,src)

