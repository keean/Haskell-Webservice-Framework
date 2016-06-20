{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}
-- Lib/HTML/DOM.hs (C)2002 Keean Schupke
--
-- DOM Types.

module Lib.HTML.Filter (domTidy) where

import Char

import Lib.XML.Types

isBlankString :: String -> Bool
isBlankString [] = True
isBlankString (c0:cs) = if isSpace c0
	then isBlankString cs
	else False

isBlankText :: [XmlElement] -> Bool
isBlankText [] = True
isBlankText (CharData e0:es) = if isBlankString e0
	then isBlankText es
	else False
isBlankText _ = False

trimString :: String -> String
trimString "" = ""
trimString (c0:cs) = case (isSpace c0,isBlankString cs) of
	(True,True) -> ""
	(False,True) -> [c0]
	(True,False) -> trimString cs
	(False,False) -> c0:trimString' cs

trimString' :: String -> String
trimString' "" = ""
trimString' (c0:cs) = case (isSpace c0,isBlankString cs) of
	(True,True) -> ""
	(False,True) -> [c0]
	(True,False) -> c0:trimString cs
	(False,False) -> c0:trimString' cs

trimText :: [XmlElement] -> [XmlElement]
trimText [] = []
trimText (CharData e0:es) = (CharData $ trimString e0):trimText es
trimText (e0:es) = e0:trimText es

domTidy :: DOM -> DOM
domTidy ((i,Text t):ds) 
	| isBlankText t = domTidy ds
	| otherwise = (i,Text (trimText t)):domTidy ds
domTidy (d0:ds) = d0:domTidy ds
domTidy [] = []

