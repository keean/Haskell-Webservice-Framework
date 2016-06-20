{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- dom.hs (C)2001 Keean Schupke
--
--	Reference XML -> DOM parser
-- based on XML 1.0 Specification

module Lib.XML.Parser(xmlToDOM) where

import Char
import Monad
import Numeric

import Lib.Monad.MonadParser
import Lib.Monad.MonadT
import Lib.Monad.ParserT
import Lib.Parser.Parser
import Lib.XML.Types

-- Document ---------------------------------------------------------------------
-- tail recursive main parser.

xmlToDOM :: XmlRules -> String -> DOM -- [1]
xmlToDOM rules src = buildDocument (0,[]) src where

	buildDocument :: XmlDomStack -> String -> [DomNode]
	buildDocument k@(d,_) s = case parse xmlContent s of
		Just (s',e@(EmptyTag n _ _)) -> (\k'@(d',_) ->
			(d',e):buildDocument k' s') (openTagOnStack k (map toUpper n))
		Just (s',e@(STag n _ _)) -> (\m -> (\k'@(d',t') ->
			(d',e):buildDocument (if isTerminal rules m then k' else (d'+1,m:t')) s') (openTagOnStack k m)) (map toUpper n)
		Just (s',e@(ETag n _)) -> (d,e):buildDocument (closeTagOnStack k $ map toUpper n) s'
		Just (s',e) -> (d,e):buildDocument k s'
		Nothing -> case s of
			(_:_) -> [(-1,Text [CharData s])]
			_ -> []

	-- close tags on stack until isCloseOnExit is false.
	closeTagOnStack :: XmlDomStack -> String -> XmlDomStack
	closeTagOnStack (_,[]) _ = (0,[])
	closeTagOnStack k@(d,(k0:ks)) m
		| m==k0 = (d-1,ks)
		| isCloseOnExit rules (m,k0) = closeTagOnStack (d-1,ks) m  
		| otherwise = k

	-- if isCloseOnEntry is true then closeTagOnStack, then test new top of stack.
	openTagOnStack :: XmlDomStack -> String -> XmlDomStack
	openTagOnStack (_,[]) m = (0,[m])
	openTagOnStack k@(_,(t0:_)) m = if isCloseOnEntry rules (m,t0)
		then (\k' -> openTagOnStack k' m) (closeTagOnStack k t0)
		else k

	-- White Space --------------------------------------------------------------

	xmlS :: Parser String -- [3]
	xmlS = many1 (satisfy isSpace)

	-- Names & Tokens -----------------------------------------------------------

	xmlNameChar :: Parser Char -- [4]
	xmlNameChar = satisfy (\c -> isAlpha c || isDigit c || c=='.' || c=='-'
		|| c=='_' || c==':')

	xmlName :: Parser String -- [5]
	xmlName = do
		c0 <- satisfy (\c -> isAlpha c || c=='_' || c==':')
		cs <- many xmlNameChar
		return (c0:cs)

	-- xmlNames :: Parser [String] -- [6]
	-- xmlNames = do
	-- 	n0 <- xmlName
	-- 	ns <- many (do
	-- 		xmlS
	-- 		n1 <- xmlName
	-- 		return n1)
	-- 	return (n0:ns)

	xmlNmtoken :: Parser String -- [7]
	xmlNmtoken = many1 xmlNameChar

	-- xmlNmtokens :: Parser [String] -- [8]
	-- xmlNmtokens = do
	-- 	n0 <- xmlNmtoken
	-- 	ns <- many (do
	-- 		xmlS
	-- 		n1 <- xmlNmtoken
	-- 		return n1)
	-- 	return (n0:ns)

	-- Literals -----------------------------------------------------------------

	xmlEntityValue :: Parser [XmlElement] -- [9]
	xmlEntityValue = ev0 `mplus` ev1 where

		ev0 :: Parser [XmlElement]
		ev0 = do
			satisfy (=='"')
			ev <- many (xmlReference `mplus` xmlPEReference `mplus` (do
				a <- many (satisfy (\c -> c/='^' && c/= '%' && c/='&' && c/='"'))
				return (CharData a)))
			satisfy (=='"')
			return ev

		ev1 :: Parser [XmlElement]
		ev1 = do
			satisfy (=='\'')
			ev <- many (xmlReference `mplus` xmlPEReference `mplus` (do
				a <- many (satisfy (\c -> c/='^' && c/= '%' && c/='&' && c/='\''))
				return (CharData a)))
			satisfy (=='\'')
			return ev
	
	xmlAttValue :: Parser [XmlElement] -- [10]
	xmlAttValue = av0 `mplus` av1 where

		av0 :: Parser [XmlElement]
		av0 = do
			satisfy (=='"')
			av <- many (xmlReference `mplus` (do
				a <- many1 (satisfy (\c -> c/= '<' && c/='&' && c/='"'))
				return (CharData a)))
			optional (satisfy (=='"'))
			return av

		av1 :: Parser [XmlElement]
		av1 = do
			satisfy (=='\'')
			av <- many (xmlReference `mplus` (do
				a <- many1 (satisfy (\c -> c/='<' && c/='&' && c/='\''))
				return (CharData a)))
			optional (satisfy (=='\''))
			return av
		
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
		txt <- many1 (xmlReference `mplus` (do
			a <- many1 (satisfy (\c -> c/= '<' && c/='&'))
			return (CharData a)))
		return (Text txt)


	xmlCharData :: Parser XmlElement -- [14]
	xmlCharData = do
		cd <- many (satisfy (\c -> c/='<' && c/='&'))
		return (CharData cd)

	-- Comments -----------------------------------------------------------------

	xmlComment :: Parser XmlElement -- [15]
	xmlComment = do
		literal item "--"
		c <- untilP (literal item "-->") item
		v <- optional (literal item "-->")
		return (case v of
			Just _ -> Comment c XML1_0
			_ -> Comment c (NonCompliant "Badly terminated Comment"))

	-- Processing Instructions --------------------------------------------------

	xmlPI :: Parser XmlElement -- [16]
	xmlPI = do
		n <- optional xmlName
		p <- untilP (literal item "?>") item
		v <- optional (literal item "?>")
		return (case (n,v) of
			(Just m,Just _) -> PI m p XML1_0
			(_,Just _) -> PI "" p (NonCompliant "Bad name in PI")
			(Just m,_) -> PI m p (NonCompliant "Badly terminated PI Tag")
			_ -> PI "" p (NonCompliant "Bad PI Tag"))

	-- CDATA Sections -----------------------------------------------------------

	xmlCDSect :: Parser XmlElement -- [18-21]
	xmlCDSect = do
		literal item "[CDATA["
		p <- untilP (literal item "]]>") item
		v <- optional (literal item "]]>")
		return (case v of
			Just _ -> CDSect p XML1_0
			_ -> CDSect "" (NonCompliant "Badly terminated CDATA Section"))

	-- Prolog -------------------------------------------------------------------

	xmlDecl :: Parser XmlElement -- [23]
	xmlDecl = do
		literal item "xml"
		as <- many (do
			xmlS
			xmlAttribute)
		optional xmlS
		v <- optional (literal item "?>")
		return (case v of
			Just _ -> XMLDecl as XML1_0
			_ -> XMLDecl as (NonCompliant "Bad XML Declaration Tag"))

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
		optional xmlS
		satisfy (=='=')
		optional xmlS
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
		literal item "DOCTYPE"
		name <- optional (do
			xmlS
			xmlName)
		extID <- optional (do
			xmlS
			xmlExternalID)
		optional xmlS
		optional (do
			satisfy (=='[')
			-- many (xmlMarkupDecl `mplus` xmlDeclSep `mplus` xmlS)
			satisfy (==']')
			optional xmlS)
		v <- optional (satisfy (=='>'))
		return (case (name,extID,v) of
			(Just m,Just (systemLiteral,publicLiteral,compliance),Just _) -> DocType m
				systemLiteral publicLiteral compliance
			(Just m,Just (systemLiteral,publicLiteral,compliance),_) -> DocType m
				systemLiteral publicLiteral (case compliance of
					XML1_0 -> NonCompliant "Badly terminated DocType declaration"
					NonCompliant s -> NonCompliant ("Badly terminated DocType declaration: "++s))
			(Just m,_,Just _) -> DocType m "" "" XML1_0
			_ -> DocType "" "" "" (NonCompliant "Bad DocTypeDecl"))

	xmlDeclSep :: Parser XmlElement
	xmlDeclSep = (do
		s <- xmlS
		return (CharData s)) `mplus` xmlPEReference

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

	xmlSDDecl :: Parser Bool -- [32]
	xmlSDDecl = (do
		xmlS
		literal item "standalone"
		xmlEq
		bl <- (bool '"' `mplus` bool '\'')
		return bl) where 
	
		bool :: Char -> Parser Bool
		bool q = do
			satisfy (==q)
			b <- (do
				literal (item +> toLower) "yes"
				return True)
				`mplus` (do
				literal (item +> toLower) "no" 
				return False)
			satisfy (==q)
			return b

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
		n <- optional xmlName
		a <- many (do
			xmlS
			xmlAttribute)
		optional xmlS
		e <- optional (satisfy (=='/'))
		v <- optional (satisfy (=='>')) 
		return (case (n,e,v) of
		 	(Just m,Just _,Just _) -> EmptyTag m a XML1_0
		 	(Just m,_,Just _) -> STag m a XML1_0
			(Just m,_,_) -> STag m a (NonCompliant "STag not closed correctly")
			_ -> STag "" [] (NonCompliant "Bad Start Tag"))

	xmlAttribute :: Parser XmlAttribute -- [41]
	xmlAttribute = (do
		n <- xmlName
		v <- optional (do
			xmlEq
			xmlAttValue)
		return (case v of
			Just w -> XmlAttribute (n,w)
			_ -> XmlAttribute (n,[CharData ""])))

	-- End-Tag ------------------------------------------------------------------

	xmlETag :: Parser XmlElement -- [42]
	xmlETag = do
		satisfy (=='/')
		n <- optional xmlName
		optional xmlS
		v <- optional (satisfy (=='>'))
		return (case (n,v) of
			(Just m,Just _) -> ETag m XML1_0
			(Just m,_) -> ETag m (NonCompliant "ETag not closed correctly")
			_ -> ETag "" (NonCompliant "Bad End Tag"))

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
	xmlCharRef = (do
		literal item "&#"
		n <- many (satisfy isDigit)
		v <- optional (satisfy (==';'))
		return (case (n,v) of
			((_:_),Just _) -> CharRef (read n) XML1_0
			_ -> CharRef 32 (NonCompliant "Bad CharRef"))) `mplus` (do
		literal item "&#x"
		n <- many (satisfy (\c -> isDigit c || c=='A' || c=='a' || c=='B'
			|| c=='b' || c=='C' || c=='c' || c=='D' || c=='d' || c=='E'
			|| c=='e' || c=='F' || c=='f'))
		v <- optional (satisfy (==';'))
		return (case (n,v) of 
			((_:_),Just _) -> case readHex n of
				[(a,_)] -> CharRef a XML1_0
				_ -> CharRef 32 (NonCompliant "Bad hex in CharRef")
			_ -> CharRef 32 (NonCompliant "Bad hex CharRef")))

	-- Entity Reference --------------------------------------------------------

	xmlReference :: Parser XmlElement -- [67]
	xmlReference = do
		tag <- xmlEntityRef `mplus` xmlCharRef
		return $! tag

	xmlEntityRef :: Parser XmlElement -- [68]
	xmlEntityRef = do
		satisfy (=='&')
		n <- optional xmlName
		v <- optional (satisfy (==';'))
		return (case (n,v) of
			(Just m,Just _) -> EntityRef m XML1_0
			(Just m,_) -> EntityRef m (NonCompliant "EntityRef not terminated correctly")
			_ -> EntityRef "" (NonCompliant "Bad EntityRef"))

	xmlPEReference :: Parser XmlElement -- [69]
	xmlPEReference = do
		satisfy (=='%')
		n <- optional xmlName
		v <- optional (satisfy (==';'))
		return (case (n,v) of
			(Just m,Just _) -> PERef m XML1_0
			(Just m,_) -> PERef m (NonCompliant "PEReferece not terminated correctly")
			_ -> PERef "" (NonCompliant "Bad PEReference"))

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

	xmlExternalID :: Parser (XmlSystemLiteral,XmlPubidLiteral,Compliance) -- [75]
	xmlExternalID = (do
		literal item "SYSTEM"
		sp <- optional xmlS
		sl <- optional xmlSystemLiteral
		return (case (sp,sl) of 
			(Just _,Just s) -> (s,"",XML1_0)
			_ -> ("","",NonCompliant "SystemLiteral missing from SYSTEM ExternalID"))) `mplus` (do
		literal item "PUBLIC"
		pl <- optional (do
			xmlS
			xmlPubidLiteral)
		sl <- optional (do
			xmlS
			xmlSystemLiteral)
		return (case (sl,pl) of
			(Just s,Just p) -> (s,p,XML1_0)
			(_,Just p) -> ("",p,NonCompliant "SystemLiteral missing from PUBLIC ExternalID")
			(Just s,_) -> (s,"",NonCompliant "PubidLiteral missing from PUBLIC ExternalID")
			_ -> ("","",NonCompliant "Bad PUBLIC ExternalID"))) `mplus` (do
		return ("","",NonCompliant "Must have either PUBLIC or SYSTEM ExternalID"))

	xmlNDataDecl :: Parser String -- [76]
	xmlNDataDecl = do
		xmlS
		literal item "NDATA"
		xmlS
		xmlName

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

	xmlPublicID :: Parser (XmlPubidLiteral,Compliance) -- [83]
	xmlPublicID = do
		literal item "PUBLIC"
		s <- optional (do
			xmlS
			xmlPubidLiteral)
		return (case s of
			Just t -> (t,XML1_0)
			_ -> ("",NonCompliant "Missing Literal in PublicID"))


