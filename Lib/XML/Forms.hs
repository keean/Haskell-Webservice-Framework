-- forms.hs (C)2001 Keean Schupke
--
--		Forms module: utilities for processing forms
--
--		Notes: really we want to convert a form into an XML-Schema to keep
--		multi-choise select options. An XML-Schema can be represented as a
--		DOM but with certain tag-types.

module Lib.XML.Forms(processForm,completeElem,domToAttributes,domToInput,domToSelect,matchInput,matchSelect,
	matchForm,getOptions,getOptionsText,getInputs) where

import Data.Char

import Lib.XML.Types
import Lib.XML.Dom
import Lib.XML.Filter

collapseSelect :: Int -> Int -> DOM -> DOM
collapseSelect _ _ [] = []
collapseSelect i j d@((k,m):d') = if k<=j
	then collapseForm i d
	else case m of
		e@(MkElem (Tag,"OPTION",_)) -> (2,e):case filterDOM (moveBranch k) d' of
			DOMOut a d'' -> moveRoot 3 (extractText a) ++ collapseSelect i j d''
			DOMVoid -> []
		_ -> collapseSelect i j d'

-- takes DOM until </FORM> and extracts form elements.
collapseForm :: Int -> DOM -> DOM
collapseForm _ [] = []
collapseForm i d@((j,e):d') = if j<=i
	then _processForm d 
	else case e of
		(MkElem (Tag,"INPUT",_)) -> (1,e):collapseForm i d'
		(MkElem (Tag,"SELECT",_)) -> (1,e):collapseSelect i j d' 
		_ -> collapseForm i d'

-- filter document: form elements become attributes of document
_processForm :: DOM -> DOM
_processForm [] = []
_processForm ((i,m):d) = case m of
	e@(MkElem (Tag,"FORM",_)) -> (0,e):collapseForm i d
	_ -> _processForm d

{-# INLINE processForm #-}
processForm :: FilterDOM ()
processForm = mkFilterDOM _processForm

putAttribute :: Attribute -> [Attribute] -> [Attribute]
putAttribute q [] = [q]
putAttribute q@(MkAttribute (n,_)) (a0@(MkAttribute (n',_)):a') = if n==n'
	then q:a'
	else a0:putAttribute q a'


alterNameValue :: [Attribute] -> [Attribute] -> [Attribute]
alterNameValue a q = case getAttributeAsString "name" a of
	n@(_:_) -> case getAttributeAsString n q of
		v@(_:_) -> putAttribute (MkAttribute ("value",v)) a
		_ -> a
	_ -> a

{-# completeElem #-}
completeElem :: [Attribute] -> FilterElem ()
completeElem q = FilterElem (\e -> case e of
	(MkElem (Tag,"INPUT",a)) -> ElemOut () (MkElem (Tag,"INPUT",alterNameValue a q))
	_ -> ElemOut () e)

_domToAttributes :: String -> DOM -> [Attribute]
_domToAttributes _ [] = []
_domToAttributes s ((_,e0):d') = case e0 of
	(MkElem (Text,a,_)) -> (MkAttribute (s,a)):domToAttributes d'
	_ -> (MkAttribute (s,"")):domToAttributes d'

domToAttributes :: DOM -> [Attribute]
domToAttributes [] = []
domToAttributes ((_,e0):d') = case e0 of
	(MkElem (Tag,"INPUT",as)) -> (MkAttribute (getAttr "name" as,getAttr "value" as)):domToAttributes d'
	(MkElem (Tag,t,_)) -> _domToAttributes t d'
	_ -> domToAttributes d'

domToInputs :: DOM -> [Attribute]
domToInputs [] = []
domToInputs ((_,e0):d') = case e0 of
	(MkElem (Tag,"INPUT",as)) -> (MkAttribute (getAttr "name" as,getAttr "value" as)):domToInputs d'
	_ -> domToInputs d'

getAttr :: String -> [Attribute] -> String
getAttr _ [] = ""
getAttr a (MkAttribute (n,v):as) = if n == a then v else getAttr a as

getOptions :: Int -> DOM -> [String]
getOptions _ [] = []
getOptions i ((i0,e0):d') = if i0 > i 
	then case e0 of
		(MkElem (Tag,"OPTION",as)) -> getAttr "value" as:getOptions i d'
		_ -> getOptions i d'
	else []

getOptionsText :: Int -> DOM -> [String]
getOptionsText _ [] = []
getOptionsText i ((i0,e0):d') = if i0 > i 
	then case e0 of
		(MkElem (Tag,"OPTION",_)) -> case filterDOM (moveBranch i0) d' of
			DOMOut a d'' -> extractString a:getOptionsText i d''
			_ -> getOptionsText i d'
		_ -> getOptionsText i d'
	else []

getText :: Int -> DOM -> DOM
getText _ [] = []
getText  i ((i0,e0):d') = if i0 > i
	then (i0-i,e0):getText i d'
	else []

domForm :: (Int -> DOM -> DOM) -> String -> DOM -> DOM
domForm _ _ [] = []
domForm f s ((i0,e0):d') = case e0 of
	(MkElem (Tag,"FORM",as)) -> if getAttr "name" as == s
		then f i0 d'
		else domForm f s d'
	_ -> domForm f s d'

domToSelect :: (Int -> DOM -> [String]) -> String -> DOM -> [String]
domToSelect _ _ [] = []
domToSelect f s ((i0,e0):d') = case e0 of
	(MkElem (Tag,"SELECT",as)) -> if getAttr "name" as == s
		then f i0 d'
		else domToSelect f s d'
	_ -> domToSelect f s d'

domToInput :: String -> DOM -> String
domToInput _ [] = ""
domToInput s ((_,e0):d') = case e0 of
	(MkElem (Tag,"INPUT",as)) -> if getAttr "name" as == s
		then getAttr "value" as
		else domToInput s d'
	_ -> domToInput s d'

matchSelect :: (Int -> DOM -> [String]) -> String -> FilterDOM [String]
matchSelect f s = FilterDOM (\d -> DOMOut (domToSelect f s d) d)

matchInput :: String -> FilterDOM String
matchInput s = FilterDOM (\d -> DOMOut (domToInput s d) d)

matchForm :: String -> FilterDOM DOM
matchForm s = FilterDOM (\d -> case domForm getText s d of
	f@(_:_) -> DOMOut f d
	_ -> DOMVoid)

getInputs :: FilterDOM [Attribute]
getInputs = FilterDOM (\d -> DOMOut (domToInputs d) d)

