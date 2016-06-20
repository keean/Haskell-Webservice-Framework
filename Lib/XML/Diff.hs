-- Tree difference

module Lib.XML.Diff (xmlPostOrder) where

import Lib.XML.Types
import Lib.Data.Diff

xmlPostOrder :: DOM -> DOM
xmlPostOrder d = xmlPostStack [] d

xmlPostStack :: [XmlElement] -> DOM -> DOM
xmlPostStack [] [] = []
xmlPostStack (e0:es) [] = (length es,e0):xmlPostStack es []
xmlPostStack [] ((i0,e0):ds) = case e0 of
	(STag _ _) -> xmlPostStack [e0] ds
	_ -> (i0,e0):xmlPostStack [] ds
xmlPostStack x@(x0:xs) ((i0,e0):ds) = case e0 of
	(STag _ _) -> xmlPostStack (e0:x) ds
	(ETag _) -> (i0-1,x0):xmlPostStack xs ds
	_ -> (i0,e0):xmlPostStack x ds

xmlSiblings :: Int -> DOM -> [XmlElement]
xmlSiblings _ [] = []
xmlSiblings d ((i0,e0):es) = if i0 == d
	then e0:xmlSiblings d es
	else if i0 > d
		then xmlSiblings d es
		else []
	
