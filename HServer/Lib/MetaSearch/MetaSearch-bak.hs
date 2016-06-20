--
--		main program, uses DOM module 

module Lib.MetaSearch.MetaSearch(metaSearch,Property(..)) where
import Control.Concurrent 
import Char
import GHC.Base
import GHC.IOBase
import IO
import Lib.MetaSearch.Parser
import Lib/MetaSearch.DOM
import Lib/MetaSearch.Filter
import Lib/MetaSearch.Forest
import Lib/MetaSearch.Forms
import Lib/MetaSearch.Cookies 
import FiniteMap
import Lib/MetaSearch.Shuffle
import Int
import Lib/MetaSearch.Redirect
import Numeric
import Control.Exception as Exception
import Strategies

------------------------------------------------------------------------------
-- data types for searching


type Site = String
type Postcode = String
type PCodeList = String
type Key = String
type Agent = String
type Price = Int
type MinPrice = Price
type MaxPrice = Price
type PriceRange = (MinPrice,MaxPrice)
type PriceRanges = [PriceRange]
type Beds = Int
type MinBeds = Beds
type MaxBeds = Beds
type BedsRange = (MinBeds,MaxBeds)
type BedsRanges = [BedsRange]
type Type = String
type Address = String
type Description = String
type FormVars = [Attribute]

data Property = Property {
	propertySite :: Maybe Site,
	propertyPostcode :: Maybe Postcode,
	propertyPrice :: Maybe Price,
	propertyBeds :: Maybe Beds,
	propertyType :: Maybe Type,
	propertyAddress :: Maybe Address,
	propertyDescription :: Maybe Description,
	propertyAgent :: Maybe Agent} deriving Show

------------------------------------------------------------------------------
-- Strategies:
--		Strategies are used to force strict evaluation of results before files
--	are closed, and to force strict evaluation of results before they are pushed
-- into the mergeChannel

instance NFData a => NFData (Maybe a) where
	rnf Nothing = () 
	rnf (Just x) = rnf x `seq` () 

instance NFData Property where
	rnf (Property a b c d e f g h) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
		`seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h

instance NFData Attribute where
	rnf (MkAttribute (x,y)) = rnf x `seq` rnf y

------------------------------------------------------------------------------
-- merge results code

parsePostcodes :: Parser [Postcode]
parsePostcodes = do
	pc <- many $ do
		untilChar isAlpha
		l <- (required . whileChar) isAlpha
		a <- whileChar (=='*')
		n <- whileChar isDigit
		b <- whileChar (=='*')
		if a/="" 
			then return $ select_postcodes l
			else if b/=""
				then return $ select_postcodes (l++n)
				else return [l++n]
	return (concat pc)

findEnd :: Int -> [Maybe Property] -> [Property]
findEnd _ [] = []
findEnd i (m0:ms) = case m0 of
	Just m -> m:findEnd i ms
	Nothing -> if i==1
		then []
		else findEnd (i-1) ms

metaSearch :: String -> Int -> Int -> Int -> Int -> IO [Property]
metaSearch as pr0 pr1 bd0 bd1 = do
	case parseResult parsePostcodes as of
		Ok pc _ -> do
			hPutStr stderr $ (showChar '[' . showString (pcodeList pc) . showChar ',' . showInt pr0 . showChar '-'
				. showInt pr1 . showChar ',' . showInt bd0 . showChar '-' . showInt bd1) "]\n"
			mergeChannel <- newChan
			forkIO $ searchPureproperty "www.pureproperty.com" mergeChannel pc (pr0,pr1) (bd0,bd1)
			forkIO $ searchPrimelocation mergeChannel pc (pr0,pr1) (bd0,bd1)
			forkIO $ searchRightmove "www.rightmove.co.uk" mergeChannel pc (pr0,pr1) (bd0,bd1)
			results <- getChanContents mergeChannel
			return (findEnd 3 results)
		_ -> return []

------------------------------------------------------------------------------
-- shared definitions

makeProperty = Property {
	propertySite = Nothing,
	propertyPostcode = Nothing,
	propertyPrice = Nothing,
	propertyBeds = Nothing,
	propertyType = Nothing,
	propertyAddress = Nothing,
	propertyDescription = Nothing,
	propertyAgent = Nothing}

all_postcodes :: [Postcode]
all_postcodes = ["NW8","SE2","W3","W4","W5","CR0","E12","EC1","EC3","E1","WC2","N22","E14","HA0","SE20","N19","EN5","N5","SE9","NW1","SW12","N1","EC2","IG11","RM8","IG6","SW13","W6","SW11","W2","BR3","E6","RM9","SM6","CR4","W12","SW1W","SE6","SM2","NW3","DA17","SE1","SE16","E2","DA15","DA6","BR1","TN16","EC4","SE3","E17","WC1","WC1A","W1","TW8","W7","EN2","N11","E3","N13","HA9","NW11","SW2","SE23","SE4","BR2","SW3","SW7","NW6","W14","SW1","HA8","SW20","TW12","N7","SE15","SE5","E16","SM5","WC2H","SE7","SM3","SW10","BR6","KT9","NW2","E4","BR7","N3","NW10","SW4","E5","EC1V","EN4","DA5","SW9","NW9","RM5","SW19","N12","NW4","CR5","WC2E","UB8","TW5","RM14","N10","DA1","SW5","N8","CR1","SE19","SE26","SM7","TN14","RM10","E8","N6","SE14","SE8","W13","SE21","SW18","N14","TW14","SE22","N2","SW15","SW14","DA16","HA5","N18","N9","SE17","RM12","SE25","SE12","EN1","DA8","IG2","EC1A","TW13","N4","W1P","W1W","W1T","W1N","DA14","E10","E7","SW6","SW17","RM2","NW5","N21","UB6","HA1","SE10","SE18","E9","TW10","UB9","N15","UB7","RM3","HA2","HA3","RM4","UB3","SE24","W8","N20","UB10","SE13","EC1N","W11","TW3","TW4","IG1","TW7","CR8","SE11","W10","TW9","KT1","E11","EC2P","EC2R","EC2V","EC2Y","EC3A","EC3M","EC3N","EC3P","EC3R","EC3V","EC4A","EC4M","EC4N","EC4P","EC4R","EC4V","EC4Y","EC88","N16","N17","NW7","SE27","SE28","SE99","SW16","SW1A","SW1E","SW1H","SW1P","SW1V","SW1X","SW1Y","SW8","SW99","W1A","W1B","W1C","W1D","W1E","W1F","W1G","W1H","W1J","W1K","W1S","W1U","W9","WC1B","WC1E","WC1H","WC1N","WC1R","WC1V","WC1X","WC2A","WC2B","WC2N","WC2R","WC99","E13","E15","E18","E77","EC1M","EC1P","EC1R","EC1Y","EC2A","EC2M","EC2N","KT4","TW1","RM6","W1M","W1R","W1V","W1X","W1Y","SM4","KT3","KT2","UB5","HA6","UB2","EN3","RM13","RM1","HA4","RM7","CR2","IG3","UB1","BR5","HA7","KT6","TW11","BR4","RM17"]

select_postcodes :: String -> [Postcode]
select_postcodes [] = all_postcodes
select_postcodes pre = select_from all_postcodes pre

select_from :: [Postcode] -> String -> [Postcode]
select_from [] _ = []
select_from p [] = p
select_from (p0:ps) s = if isPrefixOf p0 s
	then p0:select_from ps s
	else select_from ps s

isPrefixOf :: String -> String -> Bool
isPrefixOf [] [] = True
isPrefixOf [] (_:_) = False
isPrefixOf (_:_) [] = True
isPrefixOf (c0:cs) (p0:ps) = if c0==p0
	then isPrefixOf cs ps
	else False

se_postcodes :: [Postcode]
se_postcodes = select_postcodes "SE"

ec_postcodes :: [Postcode]
ec_postcodes = select_postcodes "EC"

wc_postcodes :: [Postcode]
wc_postcodes = select_postcodes "WC"

rm_postcodes :: [Postcode]
rm_postcodes = select_postcodes "RM"

postcodes :: [Postcode]
postcodes = ["N14"]

pcodeList :: [Postcode] -> PCodeList
pcodeList pcds = join pcds where

	join :: [Postcode] -> PCodeList
	join (p0:ps) = foldl (\a b->a++(',':b)) p0 ps

joinLists :: [a] -> [a] -> [a]
joinLists [] b = b
joinLists (a0:as) b = a0:joinLists as b

maybeClose :: Maybe Handle -> IO ()
maybeClose (Just h) = (hClose h) `Exception.catch` (\e -> hPutStr stderr $ shows e "\n")
maybeClose Nothing = return ()

splitRange :: [a] -> Maybe ([a],[a])
splitRange l = splitRange' 1 (round ((toRational (length l))/(toRational 2.0))) l [] where

	splitRange' :: Int -> Int -> [a] -> [a] -> Maybe ([a],[a])
	splitRange' _ _ [] _ = Nothing
	splitRange' i j (l0:ls) r = case ls of
		(_:_) -> if i<j
			then splitRange' (i+1) j ls (l0:r)
			else Just (reverse (l0:r),ls)
		_ -> Nothing

mixRange :: [(a,a)] -> Maybe (a,a)
mixRange u = case unzip u of
	([],_) -> Nothing
	((l0:_),r) -> case reverse r of
		[] -> Nothing
		(r0:_) -> Just (l0,r0)

pickRange :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
pickRange _ [] = []
pickRange (min,max) [(i,j)]
	| i<=min && j>=max = [(i,j)]
	| otherwise = []
pickRange (min,max) (r0@(p0,q0):rs@((p1,_):_)) = if (if p1 < 0 then 0 else p1) <= min
	then pickRange (min,max) rs
	else (if p0 < 0 then 0 else p0,if q0 < 0 then maxInt else q0):pickMax max rs

pickMax :: Int -> [(Int,Int)] -> [(Int,Int)]
pickMax max (r0@(p0,q0):rs) = let q' = if q0 <= 0 then maxInt else q0
	in if max >= q'
		then (if p0 < 0 then 0 else p0,q'):pickMax max rs
		else []

checkRange :: Chan (Maybe Property) -> (Int,Int) -> (Int,Int) -> Property -> IO ()
checkRange mergeChannel pr@(pr0,pr1) bd@(bd0,bd1) p0 = case (propertyPrice p0,propertyBeds p0) of
	(Just p,Just b) -> if (pr0 <= p) && (pr1 >= p) && (bd0 <= b) && (bd1 >= b)
		then do
			writeChan mergeChannel (Just p0 `using` rnf)
			yield
		else return ()
	_ -> return ()

clipMin :: [Int] -> [Int]
clipMin [] = []
clipMin (i0:is)
	| i0<0 = 0:clipMin is
	| otherwise = i0:clipMin is

clipMax :: [Int] -> [Int]
clipMax [] = []
clipMax (j0:js)
	| j0<=0 = maxInt:clipMax js
	| otherwise = j0:clipMax js

findMatch :: [Int] -> [Int] -> Maybe Int
findMatch x y = fm x y where
	
	fm :: [Int] -> [Int] -> Maybe Int
	fm [] [] = Nothing
	fm (i0:[]) [] = Just i0
	fm (i0:is@(_:_)) [] = fm is y
	fm i@(i0:is) (j0:js)
		| i0 == j0 = Just i0
		| otherwise = fm i js

discardLt :: Int -> [Int] -> [Int]
discardLt _ [] = []
discardLt i j@(j0:js)
	| j0<i = discardLt i js
	| otherwise = j

makeRanges' :: [Int] -> [Int] -> [(Int,Int)]
makeRanges' [] _ = []
makeRanges' _ [] = []
makeRanges' (i0:is) j = case findMatch j is of
	Just k -> (i0,k):makeRanges' (discardLt k is) (discardLt k j)
	_ -> []

makeRanges :: [Int] -> [Int] -> [(Int,Int)]
makeRanges i j = makeRanges' (clipMin i) (clipMax j)

--------------------------------------------------------------------
------------------------------------------------------------------------------
-- primelocation

getKey :: IO (Maybe Handle,URL,Cookies,FormVars)
getKey = do
	(h,frm) <- getDocument (MkElem (Document,"http-get://www.primelocation.com/",[]))
	-- frm <- getDocument (MkElem (Document,"file:primelocation.com",[]))
	case filterDOM (do
		alter tidyDOM
		c <- getCookies
		(_,a) <- matchForm "searchForm"
		processForm
		f <- getInputs
		return (a,c,f)) frm of
		DOMOut (a,c,f) _ -> return (h,a,c,f) -- strict: forces close of file before return

getAgents :: PCodeList -> IO (Maybe Handle,[Agent])
getAgents pcds = (do
	(h,src) <- getDocument (MkElem (Document,"http-get://www.primelocation.com/agentSearch/agentSearch.cfm",[
		MkAttribute ("whichsearch","all"), MkAttribute ("searchClass","agent"),
		MkAttribute ("location",pcds)]))
	case filterDOM (do alter tidyDOM; processForm; matchSelect getOptionsText "lst") src of
		DOMOut lst _ -> return (h,map filterAgent lst)
		_ -> return (Nothing,[])) where

	filterAgent :: String -> Agent
	filterAgent as = case parseResult ( do
		untilChar (==':')
		(skipSpace . whileChar) (==':')
		b <- untilParserOrEnd (string " - " `mplus` string ", ") char
		return b) as of
			Ok r _ -> reverse r
			_ -> reverse as

getRanges :: IO (Maybe Handle,[PriceRange],[BedsRange])
getRanges = do
	(h,frm) <- getDocument (MkElem (Document,"http-get://www.primelocation.com/",[]))
	-- frm <- getDocument (MkElem (Document,"file:primelocation.com",[]))
	-- hPutStr stderr (generateXML frm)
	case filterDOM (do alter tidyDOM; matchForm "searchForm";(graft . elemIs) (MkElem (Tag,"SELECT",[]))) frm of
		DOMOut _ sel -> do
			return (h,(zip (map read $ domToSelect getOptions "minprice" sel)
				(map read $ domToSelect getOptions "maxprice" sel)),
				(zip (map read $ domToSelect getOptions "minbedrooms" sel)
				(map read $ domToSelect getOptions "maxbedrooms" sel)))
		_ -> do
			-- hPutStr stderr "[EMPTY]\n"
			return (h,[],[])

getInRange :: URL -> [Attribute] -> Maybe PriceRange -> Maybe BedsRange -> Postcode -> Cookies -> IO (Maybe Handle,DOM)
getInRange url key priceRange bedsRange postcode cookies = case priceRange of 
	Just (minPrice,maxPrice) -> case bedsRange of
		Just (minBeds,maxBeds) -> do
			-- hPutStr stderr $ (showChar '<' . showInt minBeds . showChar '-' . showInt maxBeds . showChar ',' .
			--	showInt minPrice . showChar '-' . showInt maxPrice) ">"
			-- src <- getDocument (MkElem (Document,"file:test.html",[]))
			hsrc <- getDocCook (MkElem (Document,"http-get://www.primelocation.com/"++url,
				setInputs [MkAttribute ("place",postcode), MkAttribute ("maxprice",show maxPrice),
				MkAttribute ("minprice",show minPrice), MkAttribute ("minbedrooms",show minBeds),
				MkAttribute ("maxbedrooms",show maxBeds), MkAttribute ("propertyType","ALL"),
				MkAttribute ("criteria","0"), MkAttribute ("pricePeriod","w"), MkAttribute ("keyword0p",""),
				MkAttribute ("rental","0"), MkAttribute ("searchClass","res"), MkAttribute ("Update","0"),
				MkAttribute ("diffnav","1"), MkAttribute ("popup","0")] key)) cookies
			-- hPutStr stderr (generateXML src)
			rdr <- followRedirects "http-get://www.primelocation.com" hsrc cookies
			return rdr 
		Nothing -> return (Nothing,[])
	Nothing -> return (Nothing,[])

parsePrice :: Parser String
parsePrice = do
	p <- (required . whileChar) isDigit
	whileChar (==',')
	ps <- parsePrice `mplus` return ""
	return (p++ps)

parseAgents :: [Agent] -> Parser Agent
parseAgents ps = Parser (\cs -> parse (tryAgents ps) (reverse cs)) where

	tryAgents :: [Agent] -> Parser Agent
	tryAgents [] = Parser (\_ -> Empty Error)
	tryAgents (p:ps) = Parser (\cs -> case parse (untilParser (stringNcs p) char) cs of
		Consumed (Ok m r) -> Consumed $ Ok (reverse (m++p)) (reverse r)
		Empty (Ok _ r) -> Consumed $ Ok (reverse p) (reverse r)
		_ -> parse (tryAgents ps) cs)

parseProperty :: [Agent] -> Postcode -> Parser Property
parseProperty agents pc = do
	untilParser (stringNcs "&pound;") char
	price <- (skipSpace . untilChar) isSpace
	bedrooms <- (do
		beds <- (skipSpace . whileChar) isDigit
		skipSpace $ do
			stringNcs "bedroom"
			((do satisfy (=='s');return ()) `mplus` return ())
		return beds) `mplus` (return "0")

	addr <- (skipSpace . untilParserOrEnd (string "..")) char
	agent <- (do
		string ".."
		(skipSpace . whileChar) isSpace
		untilEnd) `mplus` (return "")
	(ad,ag) <- return $ getAddress agents addr agent
	return $ makeProperty {
		propertySite = Just "PL",
		propertyPostcode = Just pc,
		propertyPrice = case getPrice price of
			p	| p >= 0 -> Just p
				| otherwise -> Nothing,
		propertyBeds = case read bedrooms of
			b	| b >= 0 -> Just b
				| otherwise -> Nothing,
		propertyAddress = Just ad,
		propertyAgent = Just ag} where
		
		getPrice :: String -> Price
		getPrice p = case parseResult parsePrice p of
			Ok p _ -> read p
			_ -> -1

		getAddress :: [Agent] -> Address -> Agent -> (Address,Agent)
		getAddress agents addr agent = if agent /= ""
			then (addr,agent)
			else case parseResult (parseAgents agents) addr of
				Ok a b -> case parse (untilParser (stringNcs pc) char) b of
					Consumed (Ok x _) -> (x,a)
					_ -> (b,a)
				_ -> case parse (untilParser (stringNcs pc) char) addr of
					Consumed (Ok x y) -> (x,y)
					Empty (Ok _ y) -> ("",y)
					_ -> (addr,agent) 

-- recursive split if results full
getPostcode :: Chan (Maybe Property) -> (Int,Int) -> (Int,Int) -> [PriceRange] -> [BedsRange] -> Postcode -> [Agent] -> IO ()
getPostcode mergeChannel pr bd prices beds pcode agents = do
	(kh,url,cookies,key) <- getKey
	(h,src) <- getInRange url key (mixRange prices) (mixRange beds) pcode cookies
	maybeClose kh
	-- hPutStr stderr $ showString (generateXML src) "\n"
	-- case filterDOM (do alter tidyDOM; matchForm "ladder"; cut 1) src of
	case filterDOM (do
		alter tidyDOM
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [2]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [0]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [3]
		cut 1) src of
		DOMOut _ frm -> do
		 -- putStr $ generateXML frm
	 		str <- return $ extractString frm
	 		case parseResult parseComplete str of
				Ok _ _ -> do
					-- hPutStr stderr "."
					extractProperties mergeChannel pr bd pcode frm agents
					maybeClose h
				_ -> case parseResult parseIncomplete str of 
					Ok _ _ -> case splitRange prices of
						Just (p0,p1) -> do
							-- hPutStr stderr "("
							maybeClose h
							getPostcode mergeChannel pr bd p0 beds pcode agents
							getPostcode mergeChannel pr bd p1 beds pcode agents
							-- hPutStr stderr ")"
						_ -> case splitRange beds of
							Just (b0,b1) -> do
								-- hPutStr stderr "["
								maybeClose h
								getPostcode mergeChannel pr bd prices b0 pcode agents
								getPostcode mergeChannel pr bd prices b1 pcode agents
								-- hPutStr stderr "]"
							_ -> do
								-- hPutStr stderr "!"
								extractProperties mergeChannel pr bd pcode frm agents
								maybeClose h
					_ -> do
						-- hPutStr stderr "X"
						maybeClose h
						return ()
		_ -> do
			-- hPutStr stderr "?"
			maybeClose h
			return ()
	where

	parseComplete :: Parser ()
	parseComplete = do
		untilChar (=='(')
		(required . whileChar) (=='(')
		stringNcs "complete)"
		return ()

	parseIncomplete :: Parser ()
	parseIncomplete = do
		untilChar (=='(')
		(required . whileChar) (=='(')
		stringNcs "incomplete)"
		return ()

extractProperties :: Chan (Maybe Property) -> PriceRange -> BedsRange -> Postcode
	-> DOM -> [Agent] -> IO ()
extractProperties _ _ _ _ [] _ = return ()
extractProperties mergeChannel pr bd pcode ((i0,e0):ds) agents = case e0 of
	(MkElem (Tag,"SELECT",as))
		| getAttributeAsString "name" as == "propertyid" -> extractProperty mergeChannel pr bd pcode ds agents i0
		| otherwise -> extractProperties mergeChannel pr bd pcode ds agents 
	_ -> extractProperties mergeChannel pr bd pcode ds agents

extractProperty :: Chan (Maybe Property) -> PriceRange -> BedsRange -> Postcode
	-> DOM -> [Agent] -> Int -> IO ()
extractProperty _ _ _ _ [] _ _ = return ()
extractProperty mergeChannel pr bd pcode ((i0,e0):ds) agents i
	| i0>i = case e0 of
		(MkElem (Tag,"OPTION",_)) -> do
			case filterDOM (moveBranch i0) ds of
				DOMOut a d' -> do
					getProperty mergeChannel pr bd pcode agents (extractString a)
					extractProperty mergeChannel pr bd pcode ds agents i
				_ -> extractProperty mergeChannel pr bd pcode ds agents i
		_ -> extractProperty mergeChannel pr bd pcode ds agents i
	| otherwise = return ()

getProperty :: Chan (Maybe Property) -> PriceRange -> BedsRange -> Postcode
	-> [Agent] -> String -> IO ()
getProperty mergeChannel pr bd pc agents p = case parseResult (parseProperty agents pc) p of
	Ok pp _ -> checkRange mergeChannel pr bd pp
	_ -> return ()

getProperties :: Chan (Maybe Property) -> (Int,Int) -> (Int,Int) -> [PriceRange] -> [BedsRange] -> [Postcode] -> [Agent] -> IO ()
getProperties _ _ _ [] _ _ _ = return ()
getProperties _ _ _ _ [] _ _ = return ()
getProperties _ _ _ _ _ [] _ = return ()
getProperties mergeChannel pr bd prices beds postcodes@(p0:ps) agents = do
	-- hPutStr stderr ("{"++p0)
	getPostcode mergeChannel pr bd prices beds p0 agents
	-- hPutChar stderr '}'
	getProperties mergeChannel pr bd prices beds ps agents

searchPrimelocation :: Chan (Maybe Property) -> [Postcode] -> (Int,Int) -> (Int,Int) -> IO ()
searchPrimelocation mergeChannel pcds pr bd = do
	(rh,prices',bedrooms) <- getRanges
	prices <- return $ pickRange pr prices'
	beds <- return $ pickRange bd bedrooms
	pcodes <- shuffle pcds
	(ah,agents) <- getAgents (pcodeList pcodes)
	-- agents <- insertAgents agents_tmp "" []
	-- print (pcodes,prices,beds)
	-- hPutStr stderr $ show pr
	-- hPutStr stderr $ show prices
	-- hPutStr stderr $ show bd
	-- hPutStr stderr $ show beds
	-- hPutStr stderr $ show agents
	getProperties mergeChannel pr bd prices beds pcodes agents
	maybeClose rh
	maybeClose ah
	writeChan mergeChannel Nothing

--------------------------------------------------------------------
-- search code for rightmove

searchRightmove :: String -> Chan (Maybe Property) -> [Postcode] -> PriceRange -> BedsRange -> IO ()
searchRightmove site mergeChannel pc pr bd = do
	src <- getDocument (MkElem (Document,"http://"++site++"/",[]))
	(h,rdr) <- followRedirects ("http://"++site) src []
	(_,form1) <- return $ getForm site "property_search_form" rdr
	getRMPostcodes mergeChannel site h form1 pc pr bd

-- getForm :: String -> String -> DOM -> (DOM,(URL,Cookies,FormVars))
-- getForm site _ [] = ([],("http://"++site++"/",[],[]))
-- getForm site formName d = case filterDOM (do
-- 		alter tidyDOM
-- 		c <- getCookies
-- 		(m,a) <- matchForm formName
-- 		return (m,a,c)) d of
-- 	DOMOut (m,a,c) f -> case filterDOM (do
-- 			processForm
-- 			v <- getInputs
-- 			return ((case parseResult (stringNcs "http://") a of
-- 				Ok _ a' -> "http-"++m++"://"++a'
-- 				_ -> case parseResult (stringNcs "//") a of
-- 					Ok _ a' -> "http-"++m++"://"++a'
-- 					_ -> "http-"++m++"://"++site++"/"++a),c,v)) d of
-- 		DOMOut acv _ -> (f,acv)
-- 		DOMVoid -> (f,("http://"++site++"/",[],[]))
-- 	_ -> ([],("http://"++site++"/",[],[]))

getForm :: String -> String -> DOM -> (DOM,(URL,Cookies,FormVars))
getForm site _ [] = ([],("http://"++site++"/",[],[]))
getForm site formName d = case filterDOM (do
		alter tidyDOM
		c <- getCookies
		(m,a) <- matchForm formName
		return (m,a,c)) d of
	DOMOut (m,a,c) f -> case filterDOM (do
			processForm
			v <- getInputs
			return (linkToURL site m a,c,v)) d of
		DOMOut acv _ -> (f,acv)
		DOMVoid -> (f,("http://"++site++"/",[],[]))
	_ -> ([],("http://"++site++"/",[],[]))

linkToURL :: String -> String -> String -> URL
linkToURL site m link = case parseResult (stringNcs "http://") link of
	Ok _ l -> if m /= "" then "http-"++m++"://"++l else "http://"++l
	_ -> case parseResult (stringNcs "//") link of
			Ok _ l -> if m /= "" then "http-"++m++"://"++l else "http://"++l
			_ -> if m /= "" then "http-"++m++"://"++site++"/"++link else "http://"++site++"/"++link
			
rolList :: [a] -> [a]
rolList [] = []
rolList (a0:as) = joinLists as [a0]

zeroList :: [Int]
zeroList = 0:zeroList

gotAllProperties :: DOM -> Maybe Bool
gotAllProperties d = case filterDOM ( do
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [2]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [0]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TR",[]))
		cut 1
		(graft . elemIs) (MkElem (Tag,"P",[]))
		select [1]
		cut 1
		(graft . elemIs) (MkElem (Tag,"B",[]))
		select [0]
		cut 1) d of
	DOMOut _ d' -> Just (case parseResult (stringNcs "more than") (extractString d') of
		Ok _ _ -> False
		_ -> True)
	_ -> Nothing

getPayload :: Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> Maybe Handle -> Cookies -> DOM -> IO ()
getPayload mergeChannel pc pr bd h cookies d = case filterDOM ( do
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [2]
		cut 1
		(from . elemIs) (MkElem (Tag,"IMG",[MkAttribute ("name","previous_page")]))
		xa <- (moveExc .elemIs) (MkElem (Tag,"IMG",[]))
		(graft . elemIs) (MkElem (Tag,"FORM",[MkAttribute ("name","add_to_shortlist")]))
		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		return (getAttributeAsString "href" (case xa of 
			((_,MkElem (_,_,a)):_) -> a
			_ -> []))) d of
	DOMOut (url) rst  -> case url of
		"" -> do
			getMeta mergeChannel pc pr bd rst
			maybeClose h
		_ -> do
			getMeta mergeChannel pc pr bd rst
			(nh,np) <- getDocCook (MkElem (Document,url,[])) cookies
			maybeClose h
			getPayload mergeChannel pc pr bd nh cookies (fTidyDOM np)
	_ -> do
		maybeClose h
		return ()

getMeta :: Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> DOM -> IO ()
getMeta mergeChannel pc pr bd d = case filterDOM (parseMeta pc) d of
	DOMOut m d' -> do
		checkRange mergeChannel pr bd m
		getMeta mergeChannel pc pr bd d'
	_ -> return ()

parseMeta :: Postcode -> FilterDOM Property
parseMeta pc = do
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	pr <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
	bd <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
	ty <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	(from . elemIs) (MkElem (Tag,"P",[]))
	ad <- (moveInc . elemIs) (MkElem (Tag,"P",[]))
	de <- (moveInc . elemIs) (MkElem (Tag,"P",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	ag <- (moveExc . elemIs) (MkElem (Tag,"TABLE",[]))
	return $! makeProperty {
		propertySite = Just "RM",
		propertyPostcode = Just pc,
		propertyPrice = extractPrice pr,
		propertyBeds = extractBeds bd,
		propertyType = extractLiteral ty,
		propertyAddress = extractLiteral ad,
		propertyDescription = extractLiteral de,
		propertyAgent = extractLiteral ag}

-- extractPrice :: DOM -> Maybe Int
-- extractPrice d = case parseResult ( do
-- 		untilParser (stringNcs "&pound;") char
-- 		untilChar isDigit
-- 		i <- parsePrice
-- 		return i) (extractString d) of
-- 	Ok i _ -> case i of 
-- 			[] -> Nothing
-- 			i@(i0:_)	| isDigit i0 -> Just (read i)
-- 						| otherwise -> Nothing
-- 	_ -> Nothing
		
-- extractBeds :: DOM -> Maybe Int
-- extractBeds d = case parseResult ( do
-- 		untilChar isDigit
-- 		i <- (required . whileChar) isDigit
-- 		return i) (extractString d) of
-- 	Ok i _ -> case i of
-- 			[] -> Nothing
-- 			i@(i0:_)	| isDigit i0 -> Just (read i)
-- 						| otherwise -> Nothing
-- 	_ -> Nothing

-- extractLiteral :: DOM -> Maybe String
-- extractLiteral [] = Nothing
--	extractLiteral d = Just (extractString d)

getRMPostcodes :: Chan (Maybe Property) -> String -> Maybe Handle -> (URL,Cookies,FormVars)
	-> [Postcode] -> PriceRange -> BedsRange -> IO ()
getRMPostcodes mergeChannel _ h _ [] _ _ = do
	writeChan mergeChannel Nothing
	maybeClose h
getRMPostcodes mergeChannel site h form1@(action,cookies,vars) (p0:ps) pr bd = do
	(h',src) <- getDocCook (MkElem (Document,action,setInputs [MkAttribute ("s_lo",p0)] vars)) cookies
	maybeClose h
	(f2,form2@(action2,cook2,vars2)) <- return $ getForm site "modify_search_criteria_form" src
	beds <- return $ pickRange bd $ zip (map read $ domToSelect getOptions "mi_b" f2)
		zeroList
	price <- return $ pickRange pr $ zip (map read $ domToSelect getOptions "mi_p" f2)
		(map read $ rolList $ domToSelect getOptions "ma_p" f2)
	-- hPutStr stderr $ shows form2 "\n"
	-- hPutStr stderr $ shows beds "\n"
	-- hPutStr stderr $ shows price "\n"
	binarySubdivision mergeChannel p0 pr bd h' (action2,setInputs cook2 cookies,vars2) beds price
	getRMPostcodes mergeChannel site Nothing form1 ps pr bd

binarySubdivision :: Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> Maybe Handle -> (URL,Cookies,FormVars)
	-> BedsRanges -> PriceRanges -> IO ()
binarySubdivision mergeChannel pc pr bd h submit@(_,cookies,_) beds price = do
	(rh,results) <- getRMInRange h submit (mixRange beds) (mixRange price)
	case gotAllProperties results of
		Just True -> getPayload mergeChannel pc pr bd rh cookies results
		Just False -> case splitRange price of
			Just (p0,p1) -> do 
				maybeClose rh
				binarySubdivision mergeChannel pc pr bd Nothing submit beds p0
				binarySubdivision mergeChannel pc pr bd Nothing submit beds p1
			_ -> do
				hPutStr stderr "too many properties in range\n"
				getPayload mergeChannel pc pr bd rh cookies results
		Nothing -> hPutStr stderr "rightmove site error - unable to find property count\n"
	
getRMInRange :: Maybe Handle -> (URL,Cookies,FormVars) -> Maybe BedsRange -> Maybe PriceRange
	-> IO (Maybe Handle,DOM)
getRMInRange oh (action,cookies,vars) bedsRange priceRange = case priceRange of 
	Just (minPrice,maxPrice) -> case bedsRange of
		Just (minBeds,maxBeds) -> do
			-- hPutStr stderr $ shows action "\n"
			-- hPutStr stderr $ shows cookies "\n"
			-- hPutStr stderr $ shows vars "\n"
			(h,src) <- getDocCook (MkElem (Document,action,setInputs [
				MkAttribute ("se_t","-1"),
				MkAttribute ("p_t","-1"),
				MkAttribute ("mi_b",showInt minBeds ""),
				MkAttribute ("mi_p",showInt minPrice ""),
				MkAttribute ("ma_p",showInt maxPrice ""),
				MkAttribute ("stc_s","true"),
				MkAttribute ("eventsubmit_dosearch","0"),
				MkAttribute ("re_s","Price") -- reverse price sort
				] vars)) cookies
			maybeClose oh
			return (h,fTidyDOM src)
		Nothing -> return (Nothing,[])
	Nothing -> return (Nothing,[])

fTidyDOM :: DOM -> DOM
fTidyDOM d = case filterDOM (alter tidyDOM) d of
	DOMOut _ d' -> d'
	_ -> d

--------------------------------------------------------------------
-- search code for pureproperty

parseLiteral :: Parser String
parseLiteral = do
	a0 <- untilParserOrEnd removeCharRef char
	as <- (tryParser (do
		removeCharRef
		bs <- parseLiteral
		return ('\n':bs))) `mplus` (return [])
	return (a0++as)

removeCharRef :: Parser ()
removeCharRef = do
	stringNcs "&#"
	(required . whileChar) isDigit
	(required . satisfy) (==';')
	return ()

stringToPrice :: String -> Maybe Int
stringToPrice s = case parseResult ( do
		(tryParser removeCharRef) `mplus` (return ())
		(tryParser $ untilParser (stringNcs "&pound;") char) `mplus` (return "")
		untilChar isDigit
		i <- parsePrice
		return i) s of
	Ok i _ -> case i of 
			[] -> Nothing
			i@(i0:_)	| isDigit i0 -> Just (read i)
						| otherwise -> Nothing
	_ -> Nothing

extractPrice :: DOM -> Maybe Int
extractPrice d = stringToPrice (extractString d)

stringToBeds :: String -> Maybe Int
stringToBeds s =  case parseResult ( do
		(tryParser removeCharRef) `mplus` (return ())
		untilChar isDigit
		i <- (required . whileChar) isDigit
		return i) s of
	Ok i _ -> case i of 
			[] -> Nothing
			i@(i0:_)	| isDigit i0 -> Just (read i)
						| otherwise -> Nothing
	_ -> Nothing
		
extractBeds :: DOM -> Maybe Int
extractBeds d = stringToBeds (extractString d)

stringToLiteral :: String -> Maybe String 
stringToLiteral s = case parseResult parseLiteral s of
	Ok l _ -> Just l
	_ -> Nothing

extractLiteral :: DOM -> Maybe String
extractLiteral [] = Nothing
extractLiteral d = stringToLiteral (extractString d) 

--------------------------------------------------------------------

getPPForm :: String -> IO (Maybe ((URL,Cookies,FormVars),PriceRanges,BedsRanges))
getPPForm site = do
	url <- return ("http://"++site++"/_flash/buy.php")
	(u2,h,src) <- getPageWithRedirects [] [] url
	case filterDOM (do
			alter tidyDOM
			c <- getCookies
			(m,a) <- matchForm "search"
			return ((m,if a=="#" then u2 else a,c) `using` rnf)) src of
		DOMOut (m,a,c) f -> case filterDOM (do
					processForm
					v <- getInputs	
					return v) f of
				DOMOut v _ -> case filterDOM (do
							minPrices <- matchSelect getOptions "Minprice"
							maxPrices <- matchSelect getOptions "Maxprice"
							minBeds <- matchSelect getOptions "Minbedrooms"
							return ((makeRanges (map read minPrices) (map read maxPrices),
								zip (map read minBeds) zeroList) `using` rnf)) f of
						DOMOut (p,b) _ -> do
							maybeClose h
							return $ Just ((linkToURL site m a,c,v),p,b)
						_ -> do
							maybeClose h
							return Nothing
				_ -> do
					maybeClose h
					return Nothing
		_ -> do
			maybeClose h
			return Nothing

getPPRanges :: String -> PriceRange -> BedsRange
	-> IO (Maybe ((URL,Cookies,FormVars),PriceRanges,BedsRanges))
getPPRanges site pr bd = do
	r <- getPPForm site
	case r of
		Just (form,p,b) -> return $ Just (form,pickRange pr p,pickRange bd b) 
		_ -> return Nothing

getLinks :: DOM -> [String]
getLinks [] = []
getLinks ((i,e):d) = case e of
	(MkElem (Tag,"A",as)) -> getAttributeAsString "href" as:getLinks d
	_ -> getLinks d

filterLinks :: FilterDOM [String]
filterLinks = FilterDOM (\d -> case d of
	((i,e):d') -> case e of
		(MkElem (Tag,"A",as)) -> case filterDOM filterLinks d' of
			DOMOut bs d'' -> DOMOut (getAttributeAsString "href" as:bs) d''
			_ -> DOMOut [] d'
		_ -> filterDOM filterLinks d'
	_-> DOMOut [] d)

getPPPostcodes :: String -> Chan (Maybe Property) -> (URL,Cookies,FormVars)
	-> [Postcode] -> PriceRanges -> BedsRanges -> PriceRange -> BedsRange -> IO ()
getPPPostcodes _ mergeChannel _ [] _ _ _ _ = writeChan mergeChannel Nothing
getPPPostcodes site mergeChannel form1 (p0:ps) pr bd p b = do
	(rh,results) <- getPPInRange form1 p0 (mixRange bd) (mixRange pr)
	ls <- return $ getPPLinkSection results
	pages <- return ((getPPPLinks (site++"/php") "get" ls) `using` rnf)
	-- hPutStr stderr $ shows pages "\n"
	ranges <- return ((getPPRLinks (site++"/php") "get" ls) `using` rnf)
	-- hPutStr stderr $ shows ranges "\n"
	getPPProperties mergeChannel p0 p b results 
	maybeClose rh
	getPPSubPage pages form1 mergeChannel p0 p b
	getPPRangePage site ranges form1 mergeChannel p0 p b
	getPPPostcodes site mergeChannel form1 ps pr bd p b

getPPRangePage :: String -> [URL] -> (URL,Cookies,FormVars) -> Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> IO ()
getPPRangePage _ [] _ _ _ _ _ = return ()
getPPRangePage site (u0:us) f1@(_,cook,vars) mergeChannel p0 p b = do
	(rh,ranr) <- getDocCook (MkElem (Document,u0,vars)) cook
	ls <- return $ getPPLinkSection ranr
	pages <- return $ getPPPLinks (site++"/php") "get" ls
	getPPProperties mergeChannel p0 p b (fTidyDOM ranr)
	maybeClose rh
	getPPSubPage pages f1 mergeChannel p0 p b 
	getPPRangePage site us f1 mergeChannel p0 p b

getPPSubPage :: [URL] -> (URL,Cookies,FormVars) -> Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> IO ()
getPPSubPage [] _ _ _ _ _ = return ()
getPPSubPage (_:[]) _ _ _ _ _ = return () -- discard print_all
getPPSubPage (u0:us@(_:_)) f1@(_,cook,vars) mergeChannel p0 p b = do
	(sh,subr) <- getDocCook (MkElem (Document,u0,vars)) cook
	getPPProperties mergeChannel p0 p b (fTidyDOM subr)
	maybeClose sh
	getPPSubPage us f1 mergeChannel p0 p b

getPPLinkSection :: DOM -> DOM
getPPLinkSection d = case filterDOM (do
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [0]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TR",[]))) d of
	DOMOut _ r -> r
	_ -> []

getPPPLinks :: String -> String -> DOM -> [String]
getPPPLinks site m d = case filterDOM (do
		select [2]
		cut 1
		(graft . elemIs) (MkElem (Tag,"A",[]))
		l <- filterLinks
		return l) d of
	DOMOut l _ -> map (linkToURL site m) l
	_ -> []

getPPRLinks :: String -> String -> DOM -> [String]
getPPRLinks site m d = case filterDOM (do
		select [0]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		(graft . elemIs) (MkElem (Tag,"TR",[]))
		selectMod 3 [1]
		(graft . elemIs) (MkElem (Tag,"A",[]))
		l <- filterLinks
		return l) d of
	DOMOut l _ -> map (linkToURL site m) l
	_ -> []

getPPProperties :: Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> DOM -> IO ()
getPPProperties mergeChannel pc pr bd d = do
	-- hPutStr stderr $ showString (generateXML d) "\n"
	case filterDOM (do
			(graft . elemIs) (MkElem (Tag,"TABLE",[]))
			discardBranches 1) d of
		DOMOut _ d' -> do
			getPPLoop mergeChannel pc pr bd d'
		_ -> return ()

getPPLoop :: Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> DOM -> IO ()
getPPLoop mergeChannel pc pr bd d = case filterDOM (getPPProperty pc) d of
	DOMOut (Just p) d' -> do
		-- hPutStr stderr $ shows p "\n"
		checkRange mergeChannel pr bd p
		getPPLoop mergeChannel pc pr bd d'
	DOMOut Nothing d' -> getPPLoop mergeChannel pc pr bd d'
	_ -> return ()

getPPProperty :: Postcode -> FilterDOM (Maybe Property)
getPPProperty pc = do
	addr <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
	(from . elemIs) (MkElem (Tag,"I",[]))
	pdat <- (moveInc . elemIs) (MkElem (Tag,"A",[]))
	agent <- (moveInc . elemIs) (MkElem (Tag,"A",[]))
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	return $ case parseResult parsePPProperty (extractString pdat) of
		Ok pp _ -> Just (pp {
			propertySite = Just "PP",
			propertyPostcode = Just pc,
			propertyAgent = extractLiteral agent,
			propertyAddress = extractLiteral addr})
		_ -> Nothing

parsePPBeds :: Parser String
parsePPBeds = (tryParser (do
	untilChar isDigit
	a <- (required . whileChar) isDigit
	whileChar isSpace
	stringNcs "bedroom"
	return a)) `mplus` (tryParser (do
	untilChar isDigit
	whileChar isDigit
	a <- parsePPBeds
	return a))
	
stringToPPBeds :: String -> Maybe Int
stringToPPBeds s = case parseResult parsePPBeds s of
	Ok i _ -> case i of
		[] -> Nothing
		i@(i0:_) | isDigit i0 -> Just (read i)
					| otherwise -> Nothing
	_ -> Nothing

parsePPProperty :: Parser Property
parsePPProperty = do
	untilParser (stringNcs "&#0149;") char
	beds <- untilParser (stringNcs "&#0149;") char
	desc <- untilParser (stringNcs "&pound;") char
	price <- untilParser (stringNcs "&#0149;") char
	return $! makeProperty {
		propertyBeds = stringToPPBeds beds,
		propertyDescription = stringToLiteral desc,
		propertyPrice = stringToPrice price}

getPPInRange :: (URL,Cookies,FormVars) -> Postcode -> Maybe BedsRange -> Maybe PriceRange
	-> IO (Maybe Handle,DOM)
getPPInRange (action,cookies,vars) pc bedsRange priceRange = case priceRange of 
	Just (minPrice,maxPrice) -> case bedsRange of
		Just (minBeds,maxBeds) -> do
			-- hPutStr stderr $ shows action "\n"
			-- hPutStr stderr $ shows cookies "\n"
			-- hPutStr stderr $ shows vars "\n"
			(h,src) <- getDocCook (MkElem (Document,action,setInputs [
				--MkAttribute ("page","1"),
				--MkAttribute ("subpage","1"),
				MkAttribute ("per_page","100"),
				MkAttribute ("postcodes",","++pc++","),
				MkAttribute ("Minprice",showInt minPrice ""),
				MkAttribute ("Maxprice",showInt maxPrice ""),
				MkAttribute ("Minbedrooms",showInt minBeds ""),
				MkAttribute ("searchType","buy")
				] vars)) cookies
			return (h,fTidyDOM src)
		Nothing -> return (Nothing,[])
	Nothing -> return (Nothing,[])

searchPureproperty :: URL -> Chan (Maybe Property) -> [Postcode] -> PriceRange -> BedsRange -> IO ()
searchPureproperty site mergeChannel pc p b = do
	r <- getPPRanges site p b
	case r of
		Just (form1@(ac,cook,vars),pr,bd) -> do
			print form1
			print pr 
			print bd
			getPPPostcodes site mergeChannel ("http-get://"++site++"/php/search.php",cook,[]) pc pr bd p b
		_ -> hPutStr stderr $ (showString "error parsing form: " . showString site) "\n"

