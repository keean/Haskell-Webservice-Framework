-- dom.hs (C)2001 Keean Schupke
--
--	Reference XML -> DOM parser
-- based on XML 1.0 Specification

module Lib.XML.Types(
	DOM,
	XmlRules(..),
	XmlElement(..),
	Attribute(..),
	XmlAttribute(..),
	DomNode,
	XmlDomStack,
	XmlSystemLiteral,
	XmlPubidLiteral,
	XmlTreeDepth,
	XmlTagName,
	ShowDOM,
	domGetTags
) where

import Char
import Lib.Monad.ParserT

-- Datatypes --------------------------------------------------------------------

data XmlRules = XmlRules {
	isTerminal :: String -> Bool,
	isCloseOnExit :: (String,String) -> Bool,
	isCloseOnEntry :: (String,String) -> Bool,
	specialParser :: String -> ParserT (String,String) Char Maybe String
}

type XmlTagName = String
type XmlSystemLiteral = String
type XmlPubidLiteral = String

newtype Attribute = MkAttribute (String,String) deriving (Show,Eq)
newtype XmlAttribute = XmlAttribute (String,[XmlElement]) deriving (Show,Eq)

data XmlElement = XMLDecl [XmlAttribute]
	| DocType XmlTagName XmlSystemLiteral XmlPubidLiteral
	| EmptyTag XmlTagName [XmlAttribute]
	| STag XmlTagName [XmlAttribute]
	| ETag XmlTagName
	| Text [XmlElement]
	| CharData String
	| CharRef Int
	| EntityRef XmlTagName
	| PERef XmlTagName
	| CDSect String
	| PI XmlTagName String
	| Comment String
	| Flush
	| Undefined
 	| Unparsed String deriving (Show,Eq)

domGetTags :: XmlTagName -> DOM -> [XmlElement]
domGetTags _ [] = []
domGetTags t ((_,tag@(STag n _)):ds) | t==n = tag:domGetTags t ds
domGetTags t ((_,tag@(EmptyTag n _)):ds) | t==n = tag:domGetTags t ds
domGetTags t (_:ds) = domGetTags t ds

type XmlTreeDepth = Int
type XmlDomStack = (XmlTreeDepth,[XmlTagName])
type DomNode = (XmlTreeDepth,XmlElement)
type DOM = [DomNode]
type ShowDOM = DOM -> DOM

