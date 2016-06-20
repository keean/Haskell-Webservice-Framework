removeCharRef :: Parser ()
removeCharRef = (tryParser (do
	stringNcs "&#"
	(required . whileChar) isDigit
	(required . satisfy) (==';')
	return ())) `mplus` (return ())

extractPrice :: DOM -> Maybe Int
extractPrice d = case parseResult ( do
		removeCharRef
		untilParser (stringNcs "&pound;") char
		untilChar isDigit
		i <- parsePrice
		return (read i)) (extractString d) of
	Ok i _ -> Just i
	_ -> Nothing
		
extractBeds :: DOM -> Maybe Int
extractBeds d = case parseResult ( do
		removeCharRef
		untilChar isDigit
		i <- (required . whileChar) isDigit
		return (read i)) (extractString d) of
	Ok i _ -> Just i
	_ -> Nothing

extractLiteral :: DOM -> Maybe String
extractLiteral [] = Nothing
extractLiteral d = case parseResult removeCharRef (extractString d) of
	Ok _ i -> Just i
	_ -> Nothing

--------------------------------------------------------------------

getPPForm :: String -> IO (Maybe (Maybe Handle,(URL,Cookies,FormVars),PriceRanges,BedsRanges))
getPPForm site = do
	url <- return ("http://"++site++"/_flash/buy.php")
	(u2,h,src) <- getPageWithRedirects [] [] url
	case filterDOM (do
			alter tidyDOM
			c <- getCookies
			(m,a) <- matchForm "search"
			return (m,if a=="#" then u2 else a,c)) src of
		DOMOut (m,a,c) f -> case filterDOM (do
					processForm
					v <- getInputs	
					return v) f of
				DOMOut v _ -> case filterDOM (do
							minPrices <- matchSelect getOptions "Minprice"
							maxPrices <- matchSelect getOptions "Maxprice"
							minBeds <- matchSelect getOptions "Minbedrooms"
							return (makeRanges (map read minPrices) (map read maxPrices),
								zip (map read minBeds) zeroList)) f of
						DOMOut (p,b) _ -> return $ Just (h,(linkToURL site m a,c,v),p,b)
						_ -> return Nothing
				_ -> return Nothing
		_ -> return Nothing

getPPRanges :: String -> PriceRange -> BedsRange
	-> IO (Maybe (Maybe Handle,(URL,Cookies,FormVars),PriceRanges,BedsRanges))
getPPRanges site pr bd = do
	r <- getPPForm site
	case r of
		Just (h,form,p,b) -> return $ Just (h,form,pickRange pr p,pickRange bd b)
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

getPPPostcodes :: String -> Maybe Handle -> (URL,Cookies,FormVars)
	-> [Postcode] -> PriceRanges -> BedsRanges -> IO ()
getPPPostcodes _ h _ [] _ _ = maybeClose h
getPPPostcodes site h form1 (p0:ps) pr bd = do
	(rh,results) <- getPPInRange form1 p0 (mixRange bd) (mixRange pr)
	ls <- return $ getPPLinkSection results
	pages <- return $ getPPPLinks ls
	hPutStr stderr $ shows pages "\n"
	ranges <- return $ getPPRLinks ls
	hPutStr stderr $ shows ranges "\n"
	props <- return $ getPPProperties p0 results 
	hPutStr stderr $ shows props "\n"
	maybeClose rh
	getPPPostcodes site h form1 ps pr bd

getPPLinkSection :: DOM -> DOM
getPPLinkSection d = case filterDOM (do
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [0]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TR",[]))) d of
	DOMOut _ r -> r
	_ -> []

getPPPLinks :: DOM -> [String]
getPPPLinks d = case filterDOM (do
		select [2]
		cut 1
		(graft . elemIs) (MkElem (Tag,"A",[]))
		l <- filterLinks
		return l) d of
	DOMOut l _ -> l
	_ -> []

getPPRLinks :: DOM -> [String]
getPPRLinks d = case filterDOM (do
		select [0]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		(graft . elemIs) (MkElem (Tag,"TR",[]))
		selectMod 3 [1]
		(graft . elemIs) (MkElem (Tag,"A",[]))
		l <- filterLinks
		return l) d of
	DOMOut l _ -> l
	_ -> []

getPPProperties :: Postcode -> DOM -> [Property]
getPPProperties pc d = case filterDOM (do
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		discardBranches 1
		props <- getPPProperty pc
		return props) d of
	DOMOut p _ -> p
	_ -> []

getPPProperty :: Postcode -> FilterDOM [Property]
getPPProperty pc = do
	(from . elemIs) (MkElem (Tag,"TD",[]))
	addr <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
	(from . elemIs) (MkElem (Tag,"I",[]))
	beds <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
	(from . elemIs) (MkElem (Tag,"DIV",[]))
	desc <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
	price <- (moveInc . elemIs) (MkElem (Tag,"A",[]))
	agent <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	nxt <- (getPPProperty pc) `mplus` (return [])
	return $ makeProperty {
		propertyPostcode = Just pc,
		propertyAddress = extractLiteral addr,
		propertyBeds = extractBeds beds,
		propertyDescription = extractLiteral desc,
		propertyPrice = extractPrice price,
		propertyAgent = extractLiteral agent}:nxt

getPPInRange :: (URL,Cookies,FormVars) -> Postcode -> Maybe BedsRange -> Maybe PriceRange
	-> IO (Maybe Handle,DOM)
getPPInRange (action,cookies,vars) pc bedsRange priceRange = case priceRange of 
	Just (minPrice,maxPrice) -> case bedsRange of
		Just (minBeds,maxBeds) -> do
			hPutStr stderr $ shows action "\n"
			hPutStr stderr $ shows cookies "\n"
			hPutStr stderr $ shows vars "\n"
			(h,src) <- getDocCook (MkElem (Document,action,setInputs [
				MkAttribute ("page","1"),
				MkAttribute ("subpage","1"),
				MkAttribute ("per_page","10"), -- use 100 for real
				MkAttribute ("postcodes",","++pc++","),
				MkAttribute ("Minprice",showInt minPrice ""),
				MkAttribute ("Maxprice",showInt maxPrice ""),
				MkAttribute ("Minbedrooms",showInt minBeds ""),
				MkAttribute ("searchType","buy")
				] vars)) cookies
			return (h,fTidyDOM src)
		Nothing -> return (Nothing,[])
	Nothing -> return (Nothing,[])

searchPureproperty :: URL -> [Postcode] -> PriceRange -> BedsRange -> IO ()
searchPureproperty site pc pr bd = do
	r <- getPPRanges site pr bd
	case r of
		Just (h,form1@(ac,cook,vars),pr,bd) -> do
			print form1
			print pr 
			print bd
			getPPPostcodes site h ("http-get://"++site++"/php/search.php",cook,[]) pc pr bd
		_ -> hPutStr stderr $ (showString "error parsing form: " . showString site) "\n"

