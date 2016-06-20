-- main.hs (C)2001 Keean Schupke
--
--		main program, uses DOM module 

module Main(main) where
import IO
import Lib.MetaSearch.Parser
import Lib.MetaSearch.DOM
import Lib.MetaSearch.Filter
import Lib.MetaSearch.Forest
import Lib.MetaSearch.Forms

filterPos :: FilterDOM DOM
filterPos = do
	(from . elemIs) (MkElem (Tag,"FONT",[]))
	(from . elemIs) (MkElem (Tag,"B",[]))
	a <- (moveExc . elemIs) (MkElem (Close,"B",[]))
	(from . elemIs) (MkElem (Tag,"BR",[]))
	b <- (moveExc . elemIs) (MkElem (Close,"FONT",[]))
	c <- tryFilter filterPos
	return ((addChild (extractText a) (parseStringToDOM Tag (mapContains [
		(["chief executive officer"],"CEO"),
		(["chairman"],"CHAIRMAN"),
		(["managing director"],"MD"),
		(["secretary"],"SECRETARY"),
		(["finance director"],"CFO")])
		(extractText b)))++c)

filterPeople :: FilterDOM DOM
filterPeople = do
	(from . textContains) (string "People")
	(from . elemIs) (MkElem (Close,"TABLE",[]))
	(from . elemIs) (MkElem (Close,"TABLE",[]))
	alter trimElem
	a <- (moveExc . textContains) (string "Contact Information")
	b <- with a filterPos 
	return b

filter_dom :: FilterDOM DOM
filter_dom = do
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	select [6]
	a <- filterPeople
	return (addChild a [(0,MkElem (Tag,"COMPANY",[MkAttribute ("source","business")]))])

fHooversPos :: FilterDOM DOM
fHooversPos = do
	(from . textContains) (string "Key Person")
	(from . elemIs) (MkElem (Tag,"BR",[]))
	(from . elemIs) (MkElem (Tag,"FONT",[]))
	a <- (moveExc . elemIs) (MkElem (Close,"FONT",[]))
	(from . elemIs) (MkElem (Tag,"FONT",[]))
	b <- (moveExc . elemIs) (MkElem (Close,"FONT",[]))
	(from . textContains) (string "More Key People")
	return (addChild (extractText b) (parseStringToDOM Tag untilEnd (extractText a)))

fHooversPeople :: FilterDOM DOM
fHooversPeople = do
	(from . textContains) (string "Key Person")
	a <- (moveExc . textContains) (string "More Key People")
	b <- with a fHooversPos
	return (extractText a)

filter_hoovers :: FilterDOM DOM
filter_hoovers = do
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	select [1]
	cut 1
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	select [4]
	cut 1
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	select [1]
	cut 1
	a <- fHooversPos
	return (addChild a [(0,MkElem (Tag,"COMPANY",[MkAttribute ("source","hoovers")]))])

parseMeta :: Parser String
parseMeta = do
	untilChar (==';')
	(skipSpace . required . whileChar) (==';')
	stringNcs "url="
	a <- untilEnd
	return a

getRedirect :: FilterDOM DOM
getRedirect = do
	alter (FilterElem (\e -> case e of
		(MkElem (Tag,"META",_)) -> ElemOut () e
		otherwise -> ElemVoid))
	select [0]
	a <- moveAll
	return (parseStringToDOM Document parseMeta (getAttributes "content" a))

tidyDOM :: FilterElem ()
tidyDOM = do
	trimElem
	dropClose
	return ()

getLinkHRef :: FilterDOM ()
getLinkHRef = do
	alter (FilterElem (\e -> case e of
		(MkElem (Tag,"A",a)) -> ElemOut () (MkElem (Document,getAttributeAsString "href" a,[]))
		otherwise -> ElemOut () e))
	return ()

filterLinks :: FilterDOM ()
filterLinks = do
	alter tidyDOM
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	select [1]
	(graft . elemIs) (MkElem (Tag,"A",[]))
	getLinkHRef
	return ()

main = do
	src <- getDocument (MkElem (Document,"http://www.jobs.cz/osoby/index-o.html",[]))
	case filterDOM filterLinks src of
		DOMOut () src' -> putStr (generateXML src')
		DOMVoid -> print "ERROR"

