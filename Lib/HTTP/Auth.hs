-- server.hs: Copyright (C)2002 Keean Schupke
--
--		HyperServer main module

module Lib.HTTP.Auth (getHandler) where

import Data.Char
import System.IO
import Data.Set
import Data.List as List
import Control.Monad
import GHC.IO.Handle
import Numeric

import Lib.Arrow.Runnable
import Lib.Monad.MonadIO
import Lib.Parser.Parser
import Lib.XML.Types
import Lib.HTML.Types
import Lib.HTML.MonadHtml
import Lib.HTML.HtmlFragmentT
import Lib.HTTP.Types
import Lib.HTTP.Parser
import Lib.HTTP.Client
import Lib.HTTP.Server
import Lib.Server.Types

lookupAttribute :: [XmlAttribute] -> String -> String
lookupAttribute [] _ = ""
lookupAttribute (XmlAttribute (n,v):as) s = if (List.map toLower s)==(List.map toLower n)
	then valueToString v ""
	else lookupAttribute as s
	where

	valueToString :: [XmlElement] -> ShowS
	valueToString [] = id
	valueToString (x0:xs) = (case x0 of
		(CharData t) -> showString t
		(EntityRef e) -> showChar '&' . showString e . showChar ';'
		_ -> id) . valueToString xs

addAttribute :: (String,String) -> [XmlAttribute] -> [XmlAttribute]
addAttribute (n,v) [] = [XmlAttribute (n,[CharData v])]
addAttribute s@(n,v) (a0@(XmlAttribute (n0,_)):as) = if (List.map toLower n)==(List.map toLower n0)
	then XmlAttribute (n,[CharData v]):as
	else a0:addAttribute s as

parseSelect :: Parser (Set String)
parseSelect = do
	a <- ps
	return (mkSet a)
	where

	ps :: Parser [String]
	ps = many $ do
		satisfy (=='(')
		a <- many1 (satisfy (/=')'))
		satisfy (==')')
		return ("(" `mplus` a `mplus` ")")

xorSelect :: String -> Set String -> Set String
xorSelect s sel = if elementOf s sel
	then delFromSet sel s
	else addToSet sel s

showSelect :: Set String -> String
showSelect s = concat (setToList s)

extractText :: URI -> DOM -> [String]
extractText _ [] = []
extractText uri d = case parse parseSelect (lookupDict uri "select") of
	Just (_,s) -> extractText' s d
	_ -> extractText' emptySet d

extractText' :: (Set String) -> DOM -> [String]
extractText' _ [] = []
extractText' select ((_,l):ns) = case l of
	(STag "BODY" _) -> extract ns 0 []
	_ -> extractText' select ns
	where

	extract :: DOM -> Int -> [Int] -> [String]
	extract [] _ _ = []
	extract ((_,e):ds) i [] = case e of
		(Text s) -> if elementOf (listToURI [i] "") select
			then (textToString s) : extract ds (i+1) []
			else extract ds (i+1) []
		(STag _ _) -> extract ds 0 [i]
		(ETag "BODY") -> extractText' select ds
		_ -> extract ds (i+1) []
	extract ((_,e):ds) i j@(j0:js) = case e of
		(Text s) -> if elementOf (listToURI (i:j) "") select
			then (textToString s) : extract ds (i+1) j
			else extract ds (i+1) j
		(STag _ _) -> extract ds 0 (i:j)
		(ETag "BODY") -> extractText' select ds
		(ETag _) -> extract ds (j0+1) js
		_ -> extract ds (i+1) j

headDOM :: URI -> DOM -> DOM
headDOM _ [] = []
headDOM src (d0@(d,e):ds) = case e of
	(STag "BODY" _) -> []
	(EmptyTag "LINK" a) -> (d,EmptyTag "LINK" (addAttribute ("href",mapURI src $ lookupAttribute a "href") a)):headDOM src ds
	(EmptyTag "SCRIPT" a) -> (d,EmptyTag "SCRIPT" (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):headDOM src ds
	(STag "SCRIPT" a) -> (d,STag "SCRIPT" (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):headDOM src ds
	_ -> d0:headDOM src ds

tailDOM :: URI -> DOM -> DOM
tailDOM _ [] = []
tailDOM src ((_,e):ds) = case e of
	(ETag "BODY") -> tailDOM' src ds
	_ -> tailDOM src ds

tailDOM' :: URI -> DOM -> DOM
tailDOM' _ [] = []
tailDOM' src (d0@(d,e):ds) = case e of
	(EmptyTag "LINK" a) -> (d,EmptyTag "LINK" (addAttribute ("href",mapURI src $ lookupAttribute a "href") a)):tailDOM src ds
	(EmptyTag "SCRIPT" a) -> (d,EmptyTag "SCRIPT" (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):tailDOM src ds
	(STag "SCRIPT" a) -> (d,STag "SCRIPT" (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):tailDOM src ds
	_ -> d0:tailDOM src ds

bodyToLink :: Bool -> URI -> URI -> DOM -> DOM
bodyToLink _ _ _ [] = []
bodyToLink highlight uri src n = case parse parseSelect (lookupDict uri "select") of
	Just (_,s) -> bodyToLink' highlight s uri src n
	_ -> bodyToLink' highlight emptySet uri src n
	
bodyToLink' :: Bool -> (Set String) -> URI -> URI -> DOM -> DOM
bodyToLink' _ _ _ _ [] = []
bodyToLink' highlight select uri src ((_,l):ns) = case l of
	(STag "BODY" _) -> textToLink highlight ns 0 []
	_ -> bodyToLink' highlight select uri src ns
	where

	textToLink :: Bool -> DOM -> Int -> [Int] -> DOM
	textToLink _ [] _ _ = []
	textToLink hl (d0@(d,e):ds) i [] = case e of
		(Text t) -> if isTextBlank t || (not hl)
			then d0:textToLink hl ds (i+1) []
			else (\ls -> (d,STag "A" (XmlAttribute ("href",[CharData $ show $ addToDict uri "select" (showSelect (xorSelect ls select))]):if elementOf ls select 
				then [XmlAttribute ("style",[CharData "color:black;background:red;text-decoration:none"])]
				else [XmlAttribute ("style",[CharData "color:black;text-decoration:none"])])):(d+1,e):(d+1,ETag "A"):textToLink hl ds (i+1) []) (listToURI [i] "")
		(STag "A" a) -> if hl 
			then textToLink hl ds 0 [i]
			else (d,STag "A" (addAttribute ("href",show $ addToDict uri "uri" (mapURI src $ lookupAttribute a "href")) a)):textToLink False ds 0 [i]
		(STag "SCRIPT" a) -> (d,STag "SCRIPT" (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):textToLink hl ds 0 [i]
		(STag _ _) -> d0:textToLink hl ds 0 [i]
		(ETag "BODY") -> bodyToLink' highlight select uri src ds
		(EmptyTag "SCRIPT" a) -> (d,STag "SCRIPT" (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):textToLink hl ds 0 [i]
		(EmptyTag "LINK" a) -> (d,EmptyTag "LINK" (addAttribute ("href",mapURI src $ lookupAttribute a "href") a)):textToLink hl ds (i+1) []
		(EmptyTag "IMG" a) -> (d,EmptyTag "IMG" (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):textToLink hl ds (i+1) []
		(EmptyTag "INPUT" a) -> case lookupAttribute a "type" of
			"image" -> (d,EmptyTag "INPUT"  (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):textToLink hl ds (i+1) []
			_ -> d0:textToLink hl ds (i+1) []
		(EmptyTag "AREA" a) -> if hl
			then textToLink hl ds (i+1) []
			else (d,EmptyTag "AREA" (addAttribute ("href",show $ addToDict uri "uri" (mapURI src $ lookupAttribute a "href")) a)):textToLink False ds (i+1) []
		_ -> d0:textToLink hl ds (i+1) []
	textToLink hl (d0@(d,e):ds) i j@(j0:js) = case e of
		(Text t) -> if isTextBlank t || (not hl)
			then d0:textToLink hl ds (j0+1) js
			else (\ls -> (d,STag "A" (XmlAttribute ("href",[CharData $ show $ addToDict uri "select" (showSelect (xorSelect ls select))]): if elementOf ls select
				then [XmlAttribute ("style",[CharData "color:black;background:red;text-decoration:none"])]
				else [XmlAttribute ("style",[CharData "color:black;text-decoration:none"])])):(d+1,e):(d+1,ETag "A"):textToLink hl ds (i+1) j) (listToURI (i:j) "")
		(STag "A" a) -> if hl
			then textToLink hl ds 0 (i:j)
			else (d,STag "A" (addAttribute ("href",show $ addToDict uri "uri" (mapURI src $ lookupAttribute a "href")) a)):textToLink False ds 0 (i:j)
		(STag "SCRIPT" a) -> (d,STag "SCRIPT" (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):textToLink hl ds 0 (i:j)
		(STag _ _) -> d0:textToLink hl ds 0 (i:j)
		(ETag "BODY") -> d0:bodyToLink' highlight select uri src ds
		(ETag "A") -> (if hl then id else (\es -> (d,ETag "A"):es)) (textToLink highlight ds (j0+1) js)
		(ETag _) -> d0:textToLink hl ds (j0+1) js
		(EmptyTag "SCRIPT" a) -> (d,STag "SCRIPT" (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):textToLink hl ds 0 (i:j)
		(EmptyTag "LINK" a) -> (d,EmptyTag "LINK" (addAttribute ("href",mapURI src $ lookupAttribute a "href") a)):textToLink hl ds (i+1) j
		(EmptyTag "IMG" a) -> (d,EmptyTag "IMG" (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):textToLink hl ds (i+1) j
		(EmptyTag "INPUT" a) -> case lookupAttribute a "type" of
			"image" -> (d,EmptyTag "INPUT"  (addAttribute ("src",mapURI src $ lookupAttribute a "src") a)):textToLink hl ds (i+1) j
			_ -> d0:textToLink hl ds (i+1) j
		(EmptyTag "AREA" a) -> if hl
			then textToLink hl ds (i+1) j
			else (d,EmptyTag "AREA" (addAttribute ("href",show $ addToDict uri "uri" (mapURI src $ lookupAttribute a "href")) a)):textToLink False ds (i+1) j
		_ -> d0:textToLink hl ds (i+1) j

mapURI :: URI -> String -> String
mapURI src uriString = case parse (httpURI $ URI {
		uriProtocol = uriProtocol src,
		uriHost = uriHost src,
		uriPort = uriPort src,
		uriPath = uriPath src,
		uriParameters = uriParameters src
	}) uriString of
		Just (_,u) -> show u
		_ -> ""

getHandler :: IO ConnectionHandler -- Application
getHandler = return $ httpHandler authHandler

authHandler :: HttpHandler
authHandler req _ = do
	hPutStr stderr $ show req
	case uriPath (requestURI req) of
		UriPath "/" -> do
			uri <- case lookupDict (requestURI req) "uri" of
				"" -> return Nothing
				u -> return $ Just (stringToURI u)
			mrd <- case uri of
				Just u -> do
					hPutStr stderr $ (shows u) "\n"
					httpGetPage req u
				_ -> return Nothing
			case mrd of
				Just (_,d) -> respondHTML req (\rsp -> run (manager req rsp uri d)) HttpOk
				_ -> respondHTML req (\rsp -> run (manager req rsp uri [])) HttpOk
		_ -> respondHTML req (\rsp -> run (htmlNotFound req rsp)) HttpNotFound

manager :: HttpRequest -> Response -> Maybe URI -> DOM -> HtmlFragmentT IO ()
manager req rsp maybeURI doc = do
	-- ioHPutStr stderr (show doc)
	htmlDoc $ case maybeURI of
		Nothing -> attrBody [MkAttribute ("topmargin","0"),MkAttribute ("leftmargin","0"),
				MkAttribute ("marginwidth","0"),MkAttribute ("marginheight","0")] $ htmlForm "" "" $ do
			attrDiv [MkAttribute ("style","background:#cccccc;")] $ uriBar req
			htmlP $ htmlText [CharData "To use this demonstration enter \"http://fry-it.com/webselect-demo/demo.html\" into the text box at the top of the page and press return."]
		Just uri -> do
			htmlPush $ headDOM uri doc
			htmlContainer (head $ domGetTags "BODY" doc) $ do
				htmlForm "" "" $ attrDiv [MkAttribute ("id","figoDiv"),MkAttribute ("style","background:#cccccc;z-index:99;")] $ do
					uriBar req
					htmlHidden "select" (lookupDict (requestURI req) "select")
					t <- return $ joinStrings "..." (extractText (requestURI req) doc) ""
					t' <- return $ "extracted: " ++ t
					if t'==""
						then return () 
						else do
							htmlSmall $ htmlText [CharData t']
							htmlBR
				write rsp
				htmlPush $ bodyToLink (lookupDict (requestURI req) "doHighlight" == "on") (requestURI req) uri doc
			htmlPush $ tailDOM uri doc
	write rsp
	write rsp

joinStrings :: String -> [String] -> ShowS
joinStrings _ [] = id
joinStrings _ (s0:[]) = showString s0
joinStrings s (s0:ss@(_:_)) = showString s0 . showString s . joinStrings s ss

uriBar :: (MonadIO m,MonadHtml m) => HttpRequest -> m ()
uriBar req = do
	attrNullImg [MkAttribute ("height","2")]
	htmlBR
	htmlNobr $ do
		htmlB (htmlNobrText " URL: ")
		attrTextEdit [MkAttribute ("size","80")] "uri" (lookupDict (requestURI req) "uri")
		htmlNobrText "  "
		attrSpan [MkAttribute ("style","background:#999999;")] $ do
			htmlNobrText " Highlight"
			htmlCheckbox "doHighlight" (lookupDict (requestURI req) "doHighlight" == "on")
			htmlNobrText " "
	htmlBR
	attrNullImg [MkAttribute ("height","4")]
	htmlBR

stringToURI :: String -> URI
stringToURI uriString = case parse (httpURL defaultURI) uriString of
	Nothing -> defaultURI
	Just (_,uri) -> uri

listToURI :: [Int] -> ShowS
listToURI is = case toURI is "" of
	"" -> id
	s -> showChar '(' . showString s . showChar ')'

toURI :: [Int] -> ShowS
toURI [] = id
toURI (c0:[])
	| c0/=0 = showInt c0
	| otherwise = id
toURI (c0:cs@(_:_)) = (case c0 of
	0 -> id
	_ -> showInt c0) . showChar '.' . toURI cs

isTextBlank :: [XmlElement] -> Bool
isTextBlank [] = True
isTextBlank (CharData t:ts) = foldl (\b c -> isSpace c && b) (case ts of
	[] -> True
	(_:_) -> isTextBlank ts) t
isTextBlank (_:_) = False
