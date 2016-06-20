{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- dom.hs (C)2001 Keean Schupke
--
--	Reference XML -> DOM parser
-- based on XML 1.0 Specification

module Lib.XML.Parser(xmlToDOM,xmlParser,xmlParsed,xmlUnparsed) where

import Char
import Monad
import Numeric

import Lib.Monad.MonadParser
import Lib.Parser.Parser
import Lib.XML.Types

-- Document ---------------------------------------------------------------------
-- tail recursive main parser.

-- for debugging:
showStack :: XmlDomStack -> ShowS
showStack (d,k) = showChar '<' . showInt d . showChar '|' . showStack' k

showStack' :: [XmlTagName] -> ShowS
showStack' [] = showChar '>'
showStack' (k:ks) = showString k . (if null ks then id else showChar ' ') . showStack' ks

xmlParsed :: [DomNode] -> [DomNode]
xmlParsed [] = []
xmlParsed (d0:ds) = case d0 of
	(_,Unparsed _) -> []
	_ -> d0:xmlParsed ds

xmlUnparsed :: [DomNode] -> String
xmlUnparsed [] = []
xmlUnparsed (d0:ds) = case d0 of
	(_,Unparsed s) -> s
	_ -> xmlUnparsed ds

xmlRules :: XmlRules
xmlRules = XmlRules {
	isTerminal = \_ -> False,
	isCloseOnEntry = \_ -> False,
	isCloseOnExit = \_ -> False,
	specialParser = \_ -> mzero
}

xmlToDOM :: String -> DOM
xmlToDOM src = xmlParser xmlRules src

xmlParser :: XmlRules -> String -> DOM -- [1]
xmlParser rules src = buildDocument (0,[]) src where

	buildDocument :: XmlDomStack -> String -> [DomNode]
	buildDocument k@(d,_) s = case parse xmlContent s of
		Just (s',EmptyTag n a) -> openTagOnStack (EmptyTag (map toUpper n) a) k s'
		Just (s',STag n a) -> (\m -> case parse (specialParser rules m) s' of
			Nothing -> if isTerminal rules m
				then openTagOnStack (EmptyTag m a) k s'
				else openTagOnStack (STag m a) k s'
			Just (s'',cs) -> (d,STag m a):(d+1,Text [CharData cs]):(d+1,ETag m):buildDocument k s'') (map toUpper n)
		Just (s',ETag n) -> closeTagOnStack (ETag (map toUpper n)) k s'
		Just (s',e) -> (d,e):buildDocument k s'
		Nothing -> case s of
			(_:_) -> [(-1,Unparsed s)]
			_ -> [(-1,Unparsed "")]

	findCloseInStack :: [XmlTagName] -> XmlTagName -> Int -> Int
	findCloseInStack [] _ _ = -1
	findCloseInStack (t0:ts) n i = if t0==n
		then i
		else if isCloseOnExit rules (n,t0)
			then findCloseInStack ts n (i+1)
			else -1

	closeTagOnStack :: XmlElement -> XmlDomStack -> String -> [DomNode]
	closeTagOnStack (ETag n) dk@(_,k) s = case findCloseInStack k n 0 of
		i	| i<0 -> buildDocument dk s
			| otherwise -> (closeTagsOnStack buildDocument) dk s i
	closeTagOnStack _ (_,[]) s = [(-1,Unparsed s)]
	closeTagOnStack e dk@(d,_) s = (d,e):buildDocument dk s

	closeTagsOnStack :: (XmlDomStack -> String -> [DomNode]) -> XmlDomStack -> String -> Int -> [DomNode]
	closeTagsOnStack f (_,[]) s i 
		| i<0 = f (0,[]) s
		| otherwise = [(-1,Unparsed s)]
	closeTagsOnStack f dk@(d,(k0:ks)) s i
		| i<0 = f dk s
		| otherwise = (d,Comment $ showStack dk ""):(d,ETag k0):closeTagsOnStack f (d-1,ks) s (i-1)
			
	findOpenInStack :: [XmlTagName] -> XmlTagName -> Int -> Int
	findOpenInStack [] _ i = i
	findOpenInStack (t0:ts) n i = if isCloseOnEntry rules (n,t0)
		then if t0==n
			then i
			else findOpenInStack ts n (i+1)
		else i-1 -- was -1

	openTagOnStack :: XmlElement -> XmlDomStack -> String -> [DomNode]
	openTagOnStack e@(STag n _) (_,[]) s = (0,e):buildDocument (1,[n]) s
	openTagOnStack e@(EmptyTag _ _) (_,[]) s = (0,e):buildDocument (0,[]) s
	openTagOnStack e@(STag n _) dk@(d,k) s = case findOpenInStack k n 0 of
		i	| i<0 -> (d,e):buildDocument (d+1,n:k) s
			| otherwise -> (closeTagsOnStack (\(d',k') s' -> (d',e):buildDocument (d'+1,n:k') s')) dk s i
	openTagOnStack e@(EmptyTag n _) dk@(d,k) s = case findOpenInStack k n 0 of
		i  | i<0 -> (d,e):buildDocument dk s
			| otherwise -> (closeTagsOnStack (\dk'@(d',_) s' -> (d',e):buildDocument dk' s')) dk s i
	openTagOnStack e dk@(d,_) s = (d,e):buildDocument dk s

	-- White Space --------------------------------------------------------------

	-- xmlS :: Parser String -- [3]
	-- xmlS = many1 (satisfy isSpace)

	xmlOptSpc :: Parser String
	xmlOptSpc = many (satisfy isSpace)

	-- Names & Tokens -----------------------------------------------------------

	xmlIsNameChar :: Char -> Bool
	xmlIsNameChar c = isAlpha c || isDigit c || c=='_' || c==':' || c=='.' || c=='-'

	-- xmlIsName :: Char -> Bool
	-- xmlIsName c = isAlpha c || c=='_' || c==':'

	-- xmlName :: Parser String -- [5]
	-- xmlName = do
	-- 	c0 <- satisfy xmlIsName
	--  	cs <- many $ satisfy xmlIsNameChar
	-- 	return (c0:cs)

	-- isXmlName :: String -> Bool
	-- isXmlName [] = False
	-- isXmlName (c0:cs) = if xmlIsName c0
	-- 	then isXmlNameChar cs
	-- 	else False
	
	isXmlNameChar :: String -> Bool
	isXmlNameChar [] = True
	isXmlNameChar (c0:cs) = if xmlIsNameChar c0
		then isXmlNameChar cs
		else False

	-- xmlNames :: Parser [String] -- [6]
	-- xmlNames = do
	-- 	n0 <- xmlName
	-- 	ns <- many (do
	-- 		xmlS
	-- 		n1 <- xmlName
	-- 		return n1)
	-- 	return (n0:ns)

	-- xmlNmtoken :: Parser String -- [7]
	-- xmlNmtoken = many1 $ satisfy xmlIsNameChar

	-- xmlNmtokens :: Parser [String] -- [8]
	-- xmlNmtokens = do
	-- 	n0 <- xmlNmtoken
	-- 	ns <- many (do
	-- 		xmlS
	-- 		n1 <- xmlNmtoken
	-- 		return n1)
	-- 	return (n0:ns)

	-- Literals -----------------------------------------------------------------

	-- xmlEntityValue :: Parser [XmlElement] -- [9]
	-- xmlEntityValue = ev0 `mplus` ev1 where

	-- 	ev0 :: Parser [XmlElement]
	-- 	ev0 = do
	-- 		satisfy (=='"')
	-- 		ev <- many (xmlReference `mplus` xmlPEReference `mplus` (do
	-- 			a <- many (satisfy (\c -> c/='^' && c/= '%' && c/='&' && c/='"'))
	-- 			return (CharData a)))
	-- 		satisfy (=='"')
	-- 		return ev

	-- 	ev1 :: Parser [XmlElement]
	-- 	ev1 = do
	-- 		satisfy (=='\'')
	-- 		ev <- many (xmlReference `mplus` xmlPEReference `mplus` (do
	-- 			a <- many (satisfy (\c -> c/='^' && c/= '%' && c/='&' && c/='\''))
	-- 			return (CharData a)))
	-- 		satisfy (=='\'')
	-- 		return ev
	
	xmlAttValue :: Parser [XmlElement] -- [10]
	xmlAttValue = (av0 `mplus` av1 `mplus` av2) where

		av0 :: Parser [XmlElement]
		av0 = do
			satisfy (=='"')
			av <- many $ xmlCharData (/='"')
			satisfy (=='"')
			return av

		av1 :: Parser [XmlElement]
		av1 = do
			satisfy (=='\'')
			av <- many $ xmlCharData (/='\'')
			satisfy (=='\'')
			return av

		av2 :: Parser [XmlElement]
		av2 = many $ xmlCharData (\c -> c /= '>' && not (isSpace c)) 

	xmlSystemLiteral :: Parser XmlSystemLiteral -- [11]
	xmlSystemLiteral = sl0 `mplus` sl1 where

		sl0 :: Parser XmlSystemLiteral
		sl0 = do
			satisfy (=='"')
			sl <- many (satisfy (\c -> c/='"'))
			satisfy (=='"')
			return sl

		sl1 :: Parser XmlSystemLiteral
		sl1 = do
			satisfy (=='\'')
			sl <- many (satisfy (\c -> c/='\''))
			satisfy (=='\'')
			return sl

	xmlPubidLiteral :: Parser XmlPubidLiteral -- [12]
	xmlPubidLiteral = pl0 `mplus` pl1 where

		pl0 :: Parser XmlPubidLiteral
		pl0 = do
			satisfy (=='"')
			pl <- many (satisfy (\c -> c==' ' || c=='\r' || c=='\n' || isAlpha c
				|| isDigit c || c=='-' || c=='(' || c==')' || c=='+' || c==','
				|| c=='.' || c=='/' || c==':' || c=='=' || c=='?' || c==';' || c=='!'
				|| c=='*' || c=='#' || c=='@' || c=='$' || c=='_' || c=='%'
				|| c=='\''))
			satisfy (=='"')
			return pl
	
		pl1 :: Parser XmlPubidLiteral
		pl1 = do
			satisfy (=='\'')
			pl <- many (satisfy (\c -> c==' ' || c=='\r' || c=='\n' || isAlpha c
				|| isDigit c || c=='-' || c=='(' || c==')' || c=='+' || c==','
				|| c=='.' || c=='/' || c==':' || c=='=' || c=='?' || c==';' || c=='!'
				|| c=='*' || c=='#' || c=='@' || c=='$' || c=='_' || c=='%' || c=='"'))
			satisfy (=='\'')
			return pl

	-- Character Data -----------------------------------------------------------

	xmlText :: Parser XmlElement
	xmlText = do
		txt <- many1 $ xmlCharData (/='<')
		return $ Text txt

	xmlCharData :: (Char -> Bool) -> Parser XmlElement -- [14]
	xmlCharData t = xmlReference `mplus` do
		a <- satisfy t
		b <- many $ satisfy (\c -> t c && c/='&')
		return $ CharData (a:b)

	-- Comments -----------------------------------------------------------------

	xmlComment :: Parser XmlElement -- [15]
	xmlComment = do
		literal item "--"
		c <- untilP (literal item "-->") item
		return $ Comment c 

	-- Processing Instructions --------------------------------------------------

	xmlPI :: Parser XmlElement -- [16]
	xmlPI = do
		n <- many1 $ satisfy (\c -> c/='>' && c/='?' && not (isSpace c))
		p <- untilP (literal item "?>") item
		return $ PI n p

	-- CDATA Sections -----------------------------------------------------------

	xmlCDSect :: Parser XmlElement -- [18-21]
	xmlCDSect = do
		literal (fmap toUpper item) "[CDATA["
		p <- untilP (literal item "]]>") item
		return $ CDSect p

	-- Prolog -------------------------------------------------------------------

	xmlDecl :: Parser XmlElement -- [23]
	xmlDecl = do
		literal (fmap toLower item) "xml"
		a <- many $ do
			xmlOptSpc
			xmlAttribute 
		return $ XMLDecl a

	-- xmlVersionInfo :: Parser Attribute -- [24]
	-- xmlVersionInfo = do
	-- 	xmlS
	-- 	string "version"
	-- 	xmlEq
	-- 	vn <- (do
	-- 		satisfy (=='"')
	-- 		v <- xmlVersionNum
	-- 		satisfy (=='"')
	-- 		return v) `mplus` (do
	-- 		satisfy (=='\'')
	-- 		v <- xmlVersionNum
	-- 		satisfy (=='\'')
	-- 		return v)
	-- 	return (MkAttribute ("version",vn))

	xmlEq :: Parser () -- [25]
	xmlEq = do
		xmlOptSpc
		satisfy (=='=')
		xmlOptSpc
		return ()

	-- xmlVersionNum :: Parser String -- [26]
	-- xmlVersionNum = many1 (satisfy (\c -> isAlpha c || isDigit c || c=='_' || c=='.' || c==':' || c=='-'))

	-- xmlMisc :: Parser XmlElement -- [27]
	-- xmlMisc = (do
	-- 	satisfy (=='<')
	-- 	tag <- (do
	-- 		satisfy (=='!')
	-- 		xmlComment) `mplus` (do
	-- 		satisfy (=='?')
	-- 		p <- xmlPI
	-- 		satisfy (=='?')
	-- 		return p)
	-- 	satisfy (=='>')
	-- 	return tag) `mplus` (do
	-- 		s <- xmlS
	-- 		return (CharData s))

	-- Document Type Definition -------------------------------------------------

	xmlDocTypeDecl :: Parser XmlElement -- [28]
	xmlDocTypeDecl = do
		literal (fmap toUpper item) "DOCTYPE"
		xmlOptSpc
		n <- many1 $ satisfy (\c -> c/='>' && c/='[' && not (isSpace c))
		x <- optional $ do
			xmlOptSpc
			xmlExternalID
		many $ satisfy (\c -> c/='>' && c/='[')
		optional (do
			satisfy (=='[')
			-- many (xmlMarkupDecl `mplus` xmlDeclSep `mplus` xmlS)
			many $ satisfy (/=']')
			satisfy (==']')
			xmlOptSpc)
		many $ satisfy (/='>')
		satisfy (=='>')
		return $ case x of
			Just (systemLiteral,publicLiteral) -> DocType n systemLiteral publicLiteral
			_ -> DocType n "" ""
		
	-- xmlDeclSep :: Parser XmlElement
	-- xmlDeclSep = (do
	-- 	s <- xmlS
	-- 	return (CharData s)) `mplus` xmlPEReference

	-- xmlMarkupDecl :: Parser String -- [29]
	-- xmlMarkupDecl = xmlElementDecl `mplus` xmlAttListDecl `mplus` xmlEntityDecl
	-- 	`mplus` xmlNotationDecl `mplus` xmlPI `mplus` xmlComment

	-- External Subset ----------------------------------------------------------

	-- xmlExtSubset :: Parser String -- [30]
	-- xmlExtSubset = do
	-- 	optional xmlTextDecl
	-- 	xmlExtSubsetDecl

	-- xmlExtSubsetDecl :: Parser String -- [31]
	-- xmlExtSubsetDecl = (xmlMarkupDecl `mplus` xmlConditionalSect `mplus`
	-- 	xmlPEReference `mplus` xmlS)

	-- Standalone Document Declaration ------------------------------------------

	-- xmlSDDecl :: Parser Bool -- [32]
	-- xmlSDDecl = (do
	-- 	xmlOptSpc
	-- 	literal (fmap toLower item) "standalone"
	-- 	xmlEq
	-- 	bl <- (bool '"' `mplus` bool '\'')
	-- 	return bl) where 
	
	-- 	bool :: Char -> Parser Bool
	-- 	bool q = do
	-- 		satisfy (==q)
	-- 		b <- (do
	-- 			literal (fmap toLower item) "yes"
	-- 			return True)
	-- 			`mplus` (do
	-- 			literal (fmap toLower item) "no" 
	-- 			return False)
	-- 		satisfy (==q)
	-- 		return b

	-- Language Identification --------------------------------------------------

	-- xmlLanguageID :: Parser String -- [33]
	-- xmlLanguageID = do
	-- 	l <- xmlLangCode
	-- 	s <- many (do
	-- 		satisfy (=='-')
	-- 		xmlSubcode)
	-- 	return (concat $ l:s)

	-- xmlLangCode :: Parser String -- [34]
	-- xmlLangCode = xmlISO639Code `mplus` xmlIanaCode `mplus` xmlUserCode

	-- xmlISO639Code :: Parser String -- [35]
	-- xmlISO639Code = do
	-- 	a <- satisfy isAlpha
	-- 	b <- satisfy isAlpha
	-- 	return [a,b]

	-- xmlIanaCode :: Parser String -- [36]
	-- xmlIanaCode = do
	-- 	a <- satisfy (\c -> c=='i' || c=='I')
	-- 	b <- satisfy (=='-')
	-- 	c <- many1 (satisfy isAlpha)
	-- 	return (a:b:c)

	-- xmlUserCode :: Parser String -- [37]
	-- xmlUserCode = do
	-- 	a <- satisfy (\c -> c=='x' || c=='X')
	-- 	b <- satisfy (=='-')
	-- 	c <- many1 (satisfy isAlpha)
	-- 	return (a:b:c)

	-- xmlSubcode :: Parser String -- [38]
	-- xmlSubcode = many1 (satisfy isAlpha)

	-- Element ------------------------------------------------------------------

	-- xmlElement :: Parser XmlElement -- [39]
	-- xmlElement = xmlEmptyElemTag `mplus` (do
	-- 	s <- xmlSTag
	-- 	c <- xmlContent
	-- 	e <- xmlETag
	-- 	return ())

	-- Start-Tag ----------------------------------------------------------------

	xmlSTag :: Parser XmlElement -- [40]
	xmlSTag = do
		n <- many1 $ satisfy (\c -> not (isSpace c) && c/='/' && c/='>')
		a <- many $ do
			xmlOptSpc
			xmlAttribute
		many $ satisfy (\c -> c/='/' && c/='>')
		x <- optional $ satisfy (=='/')
		many $ satisfy (/='>')
		satisfy (=='>')
		return $ case x of 
			Just _ -> EmptyTag n a
			_ -> STag n a 
		
	xmlAttribute :: Parser XmlAttribute -- [41]
	xmlAttribute = do
		n <- many1 $ satisfy (\c -> c/='=' && c/='/' && c/='>' && not (isSpace c))
		v <- optional $ do
			xmlEq
			xmlAttValue 
		return $ XmlAttribute (n,case v of
			Just w -> w
			_ -> [CharData ""])

	-- End-Tag ------------------------------------------------------------------

	xmlETag :: Parser XmlElement -- [42]
	xmlETag = do
		xmlOptSpc
		satisfy (=='/')
		n <- many1 $ satisfy (\c -> c/='>' && not (isSpace c))
		many $ satisfy (/='>')
		satisfy (=='>')
		return $ ETag n

	-- Content of Elements -----------------------------------------------------

	xmlContent :: Parser XmlElement -- [43++]
	xmlContent = do
		tag <- xmlTag `mplus` xmlText
		return $! tag

	xmlTag :: Parser XmlElement
	xmlTag = do
		satisfy (=='<')
		tag <- (xmlETag `mplus` xmlMeta `mplus` xmlInstrTag `mplus` xmlSTag)
		return $! tag

	xmlInstrTag :: Parser XmlElement
	xmlInstrTag = do
		satisfy (=='?')
		tag <- (xmlDecl `mplus` xmlPI)
		return $! tag

	xmlMeta :: Parser XmlElement
	xmlMeta = do
		satisfy (=='!')
		tag <- (xmlComment `mplus` xmlDocTypeDecl `mplus` xmlCDSect)
		return $! tag

	-- Tags for Empty Elements -------------------------------------------------

	-- xmlEmptyElemTag :: Parser String -- [44]
	-- xmlEmptyElemTag = do
	-- 	satisfy (=='<')
	-- 	n <- xmlName
	-- 	a <- many (do
	-- 		xmlS
	-- 		a <- xmlAttribute
	-- 		return a)
	-- 	optional xmlS
	-- 	string "/>"

	-- Element Type Declaration ------------------------------------------------

	-- xmlElementDecl :: Parser String -- [45]
	-- xmlElementDecl = do
	-- 	string "LEMENT"
	-- 	xmlS
	-- 	n <- xmlName
	-- 	xmlS
	-- 	xmlContentSpec
	-- 	optional xmlS
	-- 	satisfy '>'
	-- 	return (ElementDecl n)

	-- xmlContentSpec :: Parser String -- [46]
	-- xmlContentSpec = string "EMPTY" `mplus` string "ANY" `mplus` xmlMixed
	-- 	`mplus` xmlChildren

	-- Element-Content Models --------------------------------------------------

	-- xmlChildren :: Parser String -- [47]
	-- xmlChildren = (do
	-- 	(xmlChoice `mplus` xmlSeq)
	-- 	optional (satisfy (\c -> c=='?' || c=='*' || c=='+'))) where

	-- 	xmlCp :: Parser String -- [48]
	-- 	xmlCp = do
	-- 		(xmlName `mplus` xmlChoice `mplus` xmlSeq)
	-- 		optional (satisfy (\c -> c=='?' || c=='*' || c=='+'))

	-- 	xmlChoice :: Parser String -- [49]
	-- 	xmlChoice = do
	-- 		satisfy (=='(')
	-- 		optional xmlS
	-- 		xmlCp
	-- 		many (do
	-- 			optional xmlS
	-- 			satisfy (=='|')
	-- 			optional xmlS
	-- 			xmlCp)
	-- 		optional xmlS
	-- 		satisfy (==')')

	-- 	xmlSeq :: Parser String -- [50]
	-- 	xmlSeq = do
	-- 		satisfy (=='(')
	-- 		optional xmlS
	-- 		xmlCp
	-- 		many (do
	-- 			optional xmlS
	-- 			satisfy (==',')
	-- 			optional xmlS
	-- 			xmlCp)
	-- 		optional xmlS
	-- 		satisfy (==')')
		
	-- Mixed-Content Models ----------------------------------------------------

	-- xmlMixed :: Parser String -- [51]
	-- xmlMixed = (do
	-- 	satisfy (=='(')
	-- 	optional xmlS
	-- 	string "#PCDATA"
	-- 	many (do
	-- 		optional xmlS
	-- 		satisfy (=='|')
	-- 		optional xmlS
	-- 		n <- xmlName
	-- 		return n)
	-- 	optional xmlS
	-- 	string ")*") `mplus` (do
	-- 	satisfy (=='(')
	-- 	optional xmlS
	-- 	string "#PCDATA"
	-- 	optional xmlS
	-- 	satisfy (==')'))


	-- Attribute-List Declaration ----------------------------------------------

	-- xmlAttListDecl :: Parser String -- [52]
	-- xmlAttListDecl = do
	-- 	string "ATTLIST"
	-- 	xmlS
	-- 	n <- xmlName
	-- 	many xmlAttDef
	-- 	optional xmlS
	-- 	satisfy (=='>')
	-- 	return n

	-- xmlAttDef :: Parser String -- [53]
	-- xmlAttDef = do
	-- 	xmlS
	--		xmlName
	-- 	xmlS
	-- 	xmlAttType
	-- 	xmlS
	-- 	xmlDefaultDecl

	-- Attribute-Types ---------------------------------------------------------

	-- xmlAttType :: Parser String -- [54]
	-- xmlAttType = (xmlStringType `mplus` xmlTokenisedType `mplus` xmlEnumeratedType)

	-- xmlStringType :: Parser () -- [55]
	-- xmlStringType = string "CDATA"

	-- xmlTokenisedType :: Parser String -- [56]
	-- xmlTokenisedType = string "ID" `mplus` string "IDREF" `mplus` string "IDREFS"
	-- 	`mplus` string "ENTITY" `mplus` string "ENTITIES" `mplus` string "NMTOKEN"
	-- 	`mplus` string "NMTOKENS"

	-- xmlEnumeratedType :: Parser String -- [57]
	-- xmlEnumeratedType = xmlNotationType `mplus` xmlEnumeration

	-- xmlNotationType :: Parser String -- [58]
	-- xmlNotationType = do
	-- 	string "NOTATION"
	-- 	xmlS
	-- 	satisfy (=='(')
	-- 	optional xmlS
	-- 	xmlName
	-- 	many (do
	-- 		optional xmlS
	-- 		satisfy (=='|')
	-- 		optional xmlS
	-- 		xmlName)
	-- 	optional xmlS
	-- 	satisfy (==')')

	-- xmlEnumeration :: Parser String -- [59]
	-- xmlEnumeration = do
	-- 	satisfy (=='(')
	-- 	xmlS
	-- 	xmlNmtoken
	-- 	many (do
	-- 		optional xmlS
	-- 		satisfy (=='|')
	-- 		optional xmlS
	-- 		xmlNmtoken)
	-- 	xmlS
	-- 	satisfy (==')')

	-- Attribute Defaults ------------------------------------------------------

	-- xmlDefaultDecl :: Parser String -- [60]
	-- xmlDefaultDecl = string "#REQUIRED" `mplus` string "#IMPLIED" `mplus` (do
	-- 	optional (do
	-- 		string "#FIXED"
	-- 		xmlS)
	-- 	xmlAttValue)
	
	-- Conditional Section --------------------------------------------------------

	-- xmlConditionalSect :: Parser String -- [61]
	-- xmlConditionalSect = xmlIncludeSect `mplus` xmlIgnoreSect

	-- xmlIncludeSect :: Parser String -- [62]
	-- xmlIncludeSect = do
	-- 	string "<!["
	-- 	optional xmlS
	-- 	string "INCLUDE"
	-- 	optional xmlS
	-- 	satisfy (=='[')
	-- 	xmlExtSubsetDecl 
	-- 	string "]]>"

	-- xmlIgnoreSect :: Parser String -- [63]
	-- xmlIgnoreSect = do
	-- 	string "<!["
	-- 	optional xmlS
	-- 	string "IGNORE"
	-- 	optional xmlS
	-- 	satisfy (=='[')
	-- 	many xmlIgnoreSectContents
	-- 	string "]]>"

	-- xmlIgnoreSectContents :: Parser String -- [64]
	-- xmlIgnoreSectContents = do
	-- 	s <- xmlIgnore
	-- 	many (do
	-- 		string "<!["
	-- 		xmlIgnoreSectContents
	-- 		string "]]>"
	-- 		xmlIgnore)

	-- xmlIgnore :: Parser String -- [65]
	-- xmlIgnore = untilParser (string "<![" `mplus` string "]]>") char

	-- Character Reference -----------------------------------------------------

	xmlCharRef :: Parser XmlElement -- [66]
	xmlCharRef = do
			literal item "&#"
			n <- many1 (satisfy isDigit)
			satisfy (==';')
			return $ CharRef (read n)
		`mplus` do
			literal item "&#x"
			n <- many1 (satisfy (\c -> isDigit c || c=='A' || c=='a' || c=='B'
			         || c=='b' || c=='C' || c=='c' || c=='D' || c=='d' || c=='E'
						         || c=='e' || c=='F' || c=='f'))
			satisfy (==';')
			return $ case readHex n of
				[(a,_)] -> CharRef a 
				_ -> CharRef 32
									
	-- Entity Reference --------------------------------------------------------

	xmlReference :: Parser XmlElement -- [67]
	xmlReference = do
		tag <- (xmlCharRef `mplus` xmlEntityRef)
		return $! tag

	xmlEntityRef :: Parser XmlElement -- [68]
	xmlEntityRef = do
		satisfy (=='&')
		n <- many1 (satisfy (\c -> c/=';' && c/='/' && c/='>' && c/='?' && (not $ isSpace c)))
		satisfy (==';')
		return $ EntityRef n

	-- xmlPEReference :: Parser XmlElement -- [69]
	-- xmlPEReference = do
	-- 	satisfy (=='%')
	-- 	n <- many1 (satisfy (\c -> c/=';' && c/='/' && c/='>' && c/='?' && (not $ isSpace c)))
	-- 	satisfy (==';')
	-- 	return $ EntityRef n

	-- Entity Declaration ---------------------------------------------------------

	-- xmlEntityDecl :: Parser String -- [70]
	-- xmlEntityDecl = xmlGEDecl `mplus` xmlPEDecl

	-- xmlGEDecl :: Parser String -- [71]
	-- xmlGEDecl = do
	-- 	string "<!ENTITY"
	-- 	xmlS
	-- 	xmlName
	-- 	xmlS
	-- 	xmlEntityDef
	-- 	optional xmlS
	-- 	satisfy (=='>')

	-- xmlPEDecl :: Parser String -- [72]
	-- xmlPEDecl = do
	-- 	string "<!ENTITY"
	-- 	xmlS
	-- 	satisfy (=='%')
	-- 	xmlS
	-- 	xmlName
	-- 	xmlS
	-- 	xmlPEDef
	-- 	optional xmlS
	-- 	satisfy (=='>')

	-- xmlEntityDef :: Parser String -- [73]
	-- xmlEntityDef = xmlEntityValue `mplus` (do
	-- 	xmlExternalID
	-- 	optional xmlNDataDecl)

	-- xmlPEDef :: Parser String -- [74]
	-- xmlPEDef = xmlEntityValue `mplus` xmlExternalID

	-- External Entity Declaration ------------------------------------------------

	xmlExternalID :: Parser (XmlSystemLiteral,XmlPubidLiteral) -- [75]
	xmlExternalID = do
			literal (fmap toUpper item) "SYSTEM"
			xmlOptSpc
			sl <- optional xmlSystemLiteral
			return $ case (sl) of 
				Just s -> (s,"")
				_ -> ("","")
		`mplus` do
			literal (fmap toUpper item) "PUBLIC"
			pl <- optional (do
				xmlOptSpc
				xmlPubidLiteral)
			sl <- optional (do
				xmlOptSpc
				xmlSystemLiteral)
			return $ case (sl,pl) of
				(Just s,Just p) -> (s,p)
				(_,Just p) -> ("",p)
				(Just s,_) -> (s,"")
				_ -> ("","")

	-- xmlNDataDecl :: Parser String -- [76]
	-- xmlNDataDecl = do
	-- 	xmlOptSpc
	-- 	literal (fmap toUpper item) "NDATA"
	-- 	xmlOptSpc
	-- 	many1 $ satisfy (\c -> not (isSpace c))

	-- Text Declaration --------------------------------------------------------

	-- xmlTextDecl :: Parser String -- [77]
	-- xmlTextDecl = do
	-- 	string "<?xml"
	-- 	optional xmlVersionInfo
	-- 	xmlEncodingDecl
	-- 	optional xmlS
	-- 	string "?>"

	-- Well-Formed External Parsed Entity --------------------------------------

	-- xmlExtParsedEnt :: Parser String -- [78]
	-- xmlExtParsedEnt = do
	-- 	optional xmlTextDecl
	-- 	xmlContent

	-- xmlExtPE :: Parser String -- [79]
	-- xmlExtPE = do
	-- 	optional xmlTextDecl
	-- 	xmlExtSubsetDecl

	-- Encoding Declaration ----------------------------------------------------

	-- xmlEncodingDecl :: Parser Attribute -- [80]
	-- xmlEncodingDecl = do
	--	xmlS
	--	string "encoding"
	--	xmlEq
	--	en <- (do
	--		satisfy (=='"')
	--		e <- xmlEncName
	--		satisfy (=='"')
	--		return e) `mplus` (do
	--		satisfy (=='\'')
	--		e <- xmlEncName
	--		satisfy (=='\'')
	--		return e)
	--	return (MkAttribute ("encoding",en))

	-- xmlEncName :: Parser String -- [81]
	-- xmlEncName = do
	-- 	satisfy isAlpha
	-- 	many (satisfy (\c -> isAlpha c || isDigit c || c=='.' || c=='_' || c=='-'))

	-- Notation Declaration ----------------------------------------------------

	-- xmlNotationDecl :: Parser String -- [82]
	-- xmlNotationDecl = do
	-- 	string "<!NOTATION"
	-- 	xmlS
	-- 	n <- xmlName
	-- 	xmlS
	-- 	(xmlExternalID `mplus` xmlPublicID)
	-- 	optional xmlS
	-- 	satisfy (=='>')
	-- 	return n

	-- xmlPublicID :: Parser XmlPubidLiteral -- [83]
	-- xmlPublicID = do
	-- 	literal (fmap toUpper item) "PUBLIC"
	-- 	xmlOptSpc
	-- 	xmlPubidLiteral

