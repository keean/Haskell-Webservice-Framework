-- cookies.hs (C)2001 Keean Schupke
--
--		Cookie Monster!

module Lib.MetaSearch.Cookies(getCookies,showCookies,Cookies) where
import Char
import IO
import Lib.MetaSearch.Parser
import Lib.MetaSearch.DOM
import Lib.MetaSearch.Filter
import Lib.MetaSearch.Forest
import Lib.MetaSearch.Forms

type Cookies = [Attribute]

parseCookie :: Parser Attribute
parseCookie = do
	a <- (skipBlanks . required . untilChar) (\c -> c=='=' || c==';' || c=='\n')
	(skipBlanks . whileChar) (=='=')
	b <- (skipBlanks . untilChar) (\c -> c==';' || c=='\n')
	(skipBlanks . whileChar) (==';')
	return (MkAttribute (a,b))

cookies :: [Attribute] -> [Attribute]
cookies [] = []
cookies (MkAttribute (l0,r0):a') = if (map toLower l0) == "set-cookie"
	then case parse parseCookie r0 of
		Consumed result -> case result of
			Ok cs _ -> cs:cookies a'
			otherwise -> cookies a'
		otherwise -> cookies a'
	else cookies a'

getCookies :: FilterDOM [Attribute]
getCookies = FilterDOM (\d -> case d of
	((_,MkElem (Header,_,a)):d') -> DOMOut (cookies a) d'
	otherwise -> DOMOut [] d)

showCookies :: [Attribute] -> ShowS
showCookies [] = (\s -> s)
showCookies (MkAttribute (n,v):as) = showString n . showChar '=' . showString v . showChar '|' . showCookies as

