{-# OPTIONS -fglasgow-exts #-}

-- Haskell HTML module
-- Copyright (C) 2002 Keean Schupke

module Lib.HTML.MonadHtml(MonadHtmlBase(..),MonadHtml(..),MonadHtmlToRow(..),
	MonadRowToTable(..),MonadTableToHtml(..),MonadHtmlToTable(..),format,formatNobr,formatString,passwordEditAttr,
	textEditAttr,nullImgAttr,formatText,tdAttr,tableAttr,textToString,hiddenAttr,submitAttr,imgAttr,linkAttr) where
	
import Char
import Monad
import Lib.XML.Types

------------------------------------------------------------------------------

class Monad f => MonadHtmlBase f where
	htmlNull :: f ()
	htmlNull = return ()

class Monad f => MonadHtml f where
	htmlElement :: XmlElement -> f ()
	htmlContainer :: XmlElement -> f a -> f a
	htmlText :: [XmlElement] -> f ()
	htmlNobrText :: String -> f ()
	htmlNbspText :: String -> f ()
	htmlNobrPad :: String -> f ()
	htmlBR :: f ()
	attrBR :: [Attribute] -> f ()
	htmlDoc :: f a -> f a
	htmlHead :: f a -> f a
	htmlTitle :: String -> f ()
	htmlBody :: f a -> f a
	attrBody :: [Attribute] -> f a -> f a
	htmlBlockquote :: f a -> f a
	htmlSmall :: f a -> f a
	htmlBig :: f a -> f a
	htmlLink :: String -> f a -> f a
	attrLink :: String -> [Attribute] -> f a -> f a
	htmlNoULink :: String -> [Attribute] -> f a -> f a
	attrNoULink :: String -> [Attribute] -> [Attribute] -> f a -> f a
	htmlB :: f a -> f a
	htmlH1 :: f a -> f a
	htmlH2 :: f a -> f a
	htmlH3 :: f a -> f a
	htmlH4 :: f a -> f a
	attrFont :: [Attribute] -> f a -> f a
	attrP :: [Attribute] -> f a -> f a
	htmlForm :: String -> String -> f a -> f a
	htmlSubmit :: String -> f ()
	attrSubmit :: [Attribute] -> String -> f ()
	htmlSubmitLink :: f a -> f a
	htmlCheckbox :: String -> Bool -> f ()
	htmlHidden :: String -> String -> f ()
	attrPasswordEdit :: [Attribute] -> String -> f ()
	htmlTextEdit :: String -> String -> f ()
	attrTextEdit :: [Attribute] -> String -> String -> f ()
	htmlNullImg :: f ()
	attrNullImg :: [Attribute] -> f ()
	htmlImg :: String -> f ()
	attrImg :: [Attribute] -> String -> f ()
	htmlStyle :: f a -> f a
	htmlSub :: f a -> f a
	htmlSuper :: f a -> f a
	htmlP :: f a -> f a
	htmlCenter :: f a -> f a
	htmlNobr :: f a -> f a
	htmlImgBtn :: String -> f ()
	htmlFlush :: f ()
	htmlPop :: f ShowDOM
	htmlPush :: [DomNode] -> f ()
	htmlSpan :: f a -> f a
	htmlDiv :: f a -> f a
	attrSpan :: [Attribute] -> f a -> f a
	attrDiv :: [Attribute] -> f a -> f a

class (Monad f,Monad g) => MonadHtmlToRow f g | g -> f where
	htmlTD :: f a -> g a
	attrTD :: [Attribute] -> f a -> g a
	htmlFragmentToColumn :: f a -> g a
	
class (Monad f,Monad g) => MonadHtmlToTable f g | f -> g where
	htmlFragmentToRow :: f a -> g a

class (Monad f,Monad g) => MonadRowToTable f g | g -> f where
	htmlTR :: f a -> g a 

class (Monad f,Monad g) => MonadTableToHtml f g | g -> f where
	htmlTable :: f a -> g a
	attrTable :: [Attribute] -> f a -> g a

------------------------------------------------------------------------------

textToString :: [XmlElement] -> String
textToString [] = ""
textToString (e0:es) = case e0 of
	CharData s -> s ++ textToString es
	EntityRef r -> '&':r ++ ';':textToString es
	_ -> textToString es

format :: String -> [XmlElement] -> [XmlElement]
format [] es = es
format (c0:cs) es
	| c0 == '"' = EntityRef "quot":format cs es
	| c0 == '<' = EntityRef "lt":format cs es
	| c0 == '>' = EntityRef "gt":format cs es
	| c0 == '&' = EntityRef "amp":format cs es
	| otherwise = format' cs [c0] es

format' :: String -> String -> [XmlElement] -> [XmlElement]
format' [] d es = CharData (reverse d):es
format' (c0:cs) d es
	| c0 == '"' = CharData (reverse d):EntityRef "quot":format cs es
	| c0 == '<' = CharData (reverse d):EntityRef "lt":format cs es
	| c0 == '>' = CharData (reverse d):EntityRef "gt":format cs es
	| c0 == '&' = CharData (reverse d):EntityRef "amp":format cs es
	| otherwise = format' cs (c0:d) es

formatNobr :: String -> [XmlElement] -> [XmlElement]
formatNobr [] bs = reverse bs
formatNobr (c0:cs) bs
	| c0 == '"' = formatNobr cs $ EntityRef "quot":bs
	| c0 == '<' = formatNobr cs $ EntityRef "lt":bs
	| c0 == '>' = formatNobr cs $ EntityRef "gt":bs
	| c0 == '&' = formatNobr cs $ EntityRef "amp":bs
	| isSpace c0 = formatNobr cs $ EntityRef "nbsp":bs
	| otherwise = formatNobr' cs [c0] bs

formatNobr' :: String -> String -> [XmlElement] -> [XmlElement]
formatNobr' [] d bs = reverse $ CharData (reverse d):bs
formatNobr' (c0:cs) d bs
	| c0 == '"' = formatNobr cs $ EntityRef "quot":CharData (reverse d):bs
	| c0 == '<' = formatNobr cs $ EntityRef "lt":CharData (reverse d):bs
	| c0 == '>' = formatNobr cs $ EntityRef "gt":CharData (reverse d):bs
	| c0 == '&' = formatNobr cs $ EntityRef "amp":CharData (reverse d):bs
	| isSpace c0 = formatNobr cs $ EntityRef "nbsp":CharData (reverse d):bs
	| otherwise = formatNobr' cs (c0:d) bs

formatString :: String -> String
formatString [] = []
formatString (c0:cs)
	| c0 == '"' = '&':'q':'u':'o':'t':';':formatString cs
	| c0 == '<' = '&':'l':'t':';':formatString cs
	| c0 == '>' = '&':'g':'t':';':formatString cs
	| c0 == '&' = '&':'a':'m':'p':';':formatString cs
	| otherwise = c0:formatString cs

passwordEditAttr :: String -> [Attribute]
passwordEditAttr s = [MkAttribute ("type","password"),MkAttribute ("name",s),MkAttribute ("value","")]

textEditAttr :: String -> String -> [Attribute]
textEditAttr s d = [MkAttribute ("type","text"),MkAttribute ("name",s),MkAttribute ("value",d)]

hiddenAttr :: String -> String -> [Attribute]
hiddenAttr s d = [MkAttribute ("type","hidden"),MkAttribute ("name",s),MkAttribute ("value",d)]

imgAttr :: String -> [Attribute]
imgAttr s = [MkAttribute ("src",s)]

nullImgAttr :: [Attribute]
nullImgAttr = [MkAttribute ("alt",""),MkAttribute ("width","1"),MkAttribute ("height","1")]

formatText :: [XmlElement] -> [XmlElement]
formatText [] = []
formatText (e0:es) = case e0 of
	CharData cs -> format cs (formatText es) 
	_ -> e0:formatText es 

tdAttr :: [Attribute]
tdAttr = [MkAttribute ("width","1"),MkAttribute ("height","1"),
	MkAttribute ("valign","top"),MkAttribute ("align","left")]

tableAttr :: [Attribute]
tableAttr = [MkAttribute ("cellspacing","0"),MkAttribute ("cellpadding","0"),
	MkAttribute ("width","1"),MkAttribute ("height","1"),MkAttribute ("border","0")]

submitAttr :: String -> [Attribute]
submitAttr s = [MkAttribute ("type","submit"),MkAttribute ("name","submit"),MkAttribute ("value",s)]

linkAttr :: String -> [Attribute]
linkAttr s = [MkAttribute ("href",s)]
