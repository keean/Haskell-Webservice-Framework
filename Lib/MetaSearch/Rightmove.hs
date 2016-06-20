

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
			return ((case parseResult (stringNcs "http://") a of
				Ok _ a' -> "http-"++m++"://"++a'
				_ -> case parseResult (stringNcs "//") a of
					Ok _ a' -> "http-"++m++"://"++a'
					_ -> "http-"++m++"://"++site++"/"++a),c,v)) d of
		DOMOut acv _ -> (f,acv)
		DOMVoid -> (f,("http://"++site++"/",[],[]))
	_ -> ([],("http://"++site++"/",[],[]))

searchRightmove :: String -> IO ()
searchRightmove site = do
	src <- getDocument (MkElem (Document,"http://"++site++"/",[]))
	(h,rdr) <- followRedirects ("http://"++site) src []
	(_,form1) <- return $ getForm site "property_search_form" rdr
	getRMPostcodes site h form1 ["N1"] (100000,300000) (1,4)

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
		select [1]
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

getPayload :: Postcode -> Maybe Handle -> Cookies -> DOM -> IO ()
getPayload pc h cookies d = case filterDOM ( do
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [2]
		cut 1
		(from . elemIs) (MkElem (Tag,"IMG",[MkAttribute ("name","previous_page")]))
		xa <- (moveExc .elemIs) (MkElem (Tag,"IMG",[]))
		(graft . elemIs) (MkElem (Tag,"FORM",[MkAttribute ("name","add_to_shortlist")]))
		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		mt <- parseMeta pc
		return (getAttributeAsString "href" (case xa of 
			((_,MkElem (_,_,a)):_) -> a
			_ -> []),mt)) d of
	DOMOut (url,mt) _  -> case url of
		"" -> do
			hPutStr stderr $ shows mt "\n"
			maybeClose h
		_ -> do
			hPutStr stderr $ shows mt "\n"
			(nh,np) <- getDocCook (MkElem (Document,url,[])) cookies
			maybeClose h
			getPayload pc nh cookies (fTidyDOM np)
	_ -> do
		maybeClose h
		return ()

data Property = Property {
	propertyPostcode :: Maybe String,
	propertyPrice :: Maybe Int,
	propertyBeds :: Maybe Int,
	propertyType :: Maybe String,
	propertyAddress :: Maybe String,
	propertyDescription :: Maybe String,
	propertyAgent :: Maybe String} deriving Show

makeProperty = Property {
	propertyPostcode = Nothing,
	propertyPrice = Nothing,
	propertyBeds = Nothing,
	propertyType = Nothing,
	propertyAddress = Nothing,
	propertyDescription = Nothing,
	propertyAgent = Nothing}
	

parseMeta :: Postcode -> FilterDOM [Property]
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
	ag <- (moveExc . elemIs) (MkElem (Tag,"TABLE",[]))
	nxt <- (parseMeta pc) `mplus` (return [])
	-- return ([extractString pr,extractString bd,extractString ty,extractString ad,extractString de,
	--	extractString x]:nxt)
	return $ makeProperty {
		propertyPostcode = Just pc,
		propertyPrice = extractPrice pr,
		propertyBeds = extractBeds bd,
		propertyType = extractLiteral ty,
		propertyAddress = extractLiteral ad,
		propertyDescription = extractLiteral de,
		propertyAgent = extractLiteral ag}:nxt

extractPrice :: DOM -> Maybe Int
extractPrice d = case parseResult ( do
		untilParser (stringNcs "&pound;") char
		untilChar isDigit
		i <- parsePrice
		return (read i)) (extractString d) of
	Ok i _ -> Just i
	_ -> Nothing
		
extractBeds :: DOM -> Maybe Int
extractBeds d = case parseResult ( do
		untilChar isDigit
		i <- (required . whileChar) isDigit
		return (read i)) (extractString d) of
	Ok i _ -> Just i
	_ -> Nothing

extractLiteral :: DOM -> Maybe String
extractLiteral [] = Nothing
extractLiteral d = Just (extractString d)

getRMPostcodes :: String -> Maybe Handle -> (URL,Cookies,FormVars)
	-> [Postcode] -> (Int,Int) -> (Int,Int) -> IO ()
getRMPostcodes _ h _ [] _ _ = maybeClose h
getRMPostcodes site h form1@(action,cookies,vars) (p0:ps) pr bd = do
	(h',src) <- getDocCook (MkElem (Document,action,setInputs [MkAttribute ("s_lo",p0)] vars)) cookies
	maybeClose h
	(f2,form2@(action2,cook2,vars2)) <- return $ getForm site "modify_search_criteria_form" src
	(beds :: [(Int,Int)]) <- return $ pickRange bd $ zip (map read $ domToSelect getOptions "mi_b" f2)
		zeroList
	(price :: [(Int,Int)]) <- return $ pickRange pr $ zip (map read $ domToSelect getOptions "mi_p" f2)
		(map read $ rolList $ domToSelect getOptions "ma_p" f2)
	hPutStr stderr $ shows form2 "\n"
	hPutStr stderr $ shows beds "\n"
	hPutStr stderr $ shows price "\n"
	binarySubdivision p0 (action2,setInputs cook2 cookies,vars2) beds price
	getRMPostcodes site h' form1 ps pr bd

binarySubdivision :: Postcode -> (URL,Cookies,FormVars) -> [(Int,Int)] -> [(Int,Int)] -> IO ()
binarySubdivision pc submit@(_,cookies,_) beds price = do
	(rh,results) <- getRMInRange submit (mixRange beds) (mixRange price)
	case gotAllProperties results of
		Just _ -> getPayload pc rh cookies results
		Nothing -> hPutStr stderr "rightmove site error - unable to find property count\n"
	
getRMInRange :: (URL,Cookies,FormVars) -> Maybe BedsRange -> Maybe PriceRange -> IO (Maybe Handle,DOM)
getRMInRange (action,cookies,vars) bedsRange priceRange = case priceRange of 
	Just (minPrice,maxPrice) -> case bedsRange of
		Just (minBeds,maxBeds) -> do
			hPutStr stderr $ shows action "\n"
			hPutStr stderr $ shows cookies "\n"
			hPutStr stderr $ shows vars "\n"
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
			return (h,fTidyDOM src)
		Nothing -> return (Nothing,[])
	Nothing -> return (Nothing,[])

fTidyDOM :: DOM -> DOM
fTidyDOM d = case filterDOM (alter tidyDOM) d of
	DOMOut _ d' -> d'
	_ -> d

