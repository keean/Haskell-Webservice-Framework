-- filter.hs (C)2001 Keean Schupke
--
--		Polymorphic monadic filter

module Lib.XML.Filter where

import Char

import Lib.XML.Types

-- Tag Selectors -------------------------------------------------------------

emptyTag :: String -> XmlElement -> Bool
emptyTag n = \e -> case e of
	(EmptyTag m _) -> m == n
	_ -> False


sTag :: String -> XmlElement -> Bool
sTag n = (\e -> case e of
	STag m _ -> m == n
	_ -> False)

-- XmlElement filters --------------------------------------------------------

lookupAttribute :: XmlElement -> String -> String
lookupAttribute e = lookupAttribute' (attributes e)

attributes :: XmlElement -> [XmlAttribute]
attributes (STag _ a) = a
attributes (EmptyTag _ a) = a
attributes _ = []

lookupAttribute' :: [XmlAttribute] -> String -> String
lookupAttribute' [] _ = ""
lookupAttribute' (XmlAttribute (n,v):as) s = if (map toLower s)==(map toLower n)
	then attributeToString v ""
	else lookupAttribute' as s
	where

attributeToString :: [XmlElement] -> ShowS
attributeToString [] = id
attributeToString (x0:xs) = (case x0 of
	(CharData t) -> showString t
	(EntityRef e) -> showChar '&' . showString e . showChar ';'
	_ -> id) . attributeToString xs

-- DOM filters ---------------------------------------------------------------

graft :: (XmlElement -> Bool) -> DOM -> DOM
graft f ((d,e):dn) 
	| f e = graft' f (d+1) dn
	| otherwise = graft f dn
graft _ _ = []

graft' :: (XmlElement -> Bool) -> XmlTreeDepth -> DOM -> DOM
graft' f i ((d,e):dn)
	| d >= i = (d - i,e):graft' f i dn
	| otherwise = graft f dn
graft' _ _ _ = []

match :: (XmlElement -> Bool) -> DOM -> DOM
match f ((_,e):dn)
	| f e = (0,e):match f dn
	| otherwise = match f dn
match _ _ = []

