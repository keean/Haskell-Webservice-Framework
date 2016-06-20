------------------------------------------------------------------------------

getInts :: [String] -> [Int]
getInts [] = []
getInts (t0:ts) = case parseResult (do
		untilChar isDigit
		i <- (required . whileChar) isDigit
		return (read i)) t0 of
	Ok i _ -> i:getInts ts
	_ -> getInts ts

priceToString :: Int -> String
priceToString i = case i of
	10000000 -> "10000000+"
	_ -> showInt i ""

bedsToString :: Int -> String
bedsToString i = case i of
	6 -> "6+"
	_ -> showInt i ""

getPFForm :: String -> IO (Maybe ((URL,Cookies,FormVars),PriceRanges,BedsRanges))
getPFForm site = do
	url <- return ("http://"++site++"/")
	(u2,h,src) <- getPageWithRedirects [] [] url
	case filterDOM (do
			alter tidyDOM
			c <- getCookies
			(m,a) <- matchForm "ipfform"
			return (m,if a=="#" then u2 else a,c)) src of
		DOMOut (m,a,c) f -> case filterDOM (do
				processForm
				v <- getInputs	
				return v) f of
			DOMOut v _ -> case filterDOM (do
					minPrices <- matchSelect getOptions "Minprice"
					maxPrices <- matchSelect getOptions "Maxprice"
					minBeds <- matchSelect getOptionsText "Minbedrooms"
					maxBeds <- matchSelect getOptionsText "Maxbedrooms"
					rnf (minPrices,maxPrices,minBeds,maxBeds) `seq` return
						(makeRanges (getInts minPrices) (getInts maxPrices),
						makeRanges (getInts minBeds) (getInts maxBeds))) f of
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

searchPropertyfinder :: URL -> [Postcode] -> PriceRange -> BedsRange -> IO ()
searchPropertyfinder site pc pr bd = do
	form <- getPFForm site
	case form of
		Just f -> getPFPostcodes site f pc pr bd
		Nothing -> hPutStr stderr "could not get propertyfinder form\n"

getPFPostcodes :: String -> ((URL,Cookies,FormVars),BedsRanges,PriceRanges) -> [Postcode] -> PriceRange -> BedsRange -> IO ()
getPFPostcodes _ _ [] _ _ = return ()
getPFPostcodes site form@(submit,price,beds) (p0:ps) pr bd = do
	subdividePF site p0 pr bd submit (pickRange bd beds) (pickRange pr price)
	getPFPostcodes site form ps pr bd
	
subdividePF :: String -> Postcode -> PriceRange -> BedsRange -> (URL,Cookies,FormVars) -> BedsRanges -> PriceRanges -> IO ()
subdividePF site pc pr bd submit@(_,cookies,_) beds price = do
	(rh,results) <- getPFInRange submit pc (mixRange beds) (mixRange price)
	case gotAllPFProperties results of
		Just True -> getPFPages site pc pr bd rh cookies results
		Just False -> case splitRange price of
			Just (p0,p1) -> do
				maybeClose rh
				subdividePF site pc pr bd submit beds p0
				subdividePF site pc pr bd submit beds p1
			_ -> case splitRange beds of
				Just (b0,b1) -> do
					maybeClose rh
					subdividePF site pc pr bd submit b0 price
					subdividePF site pc pr bd submit b1 price
				_ -> do
					hPutStr stderr "propertyfinder: too many properties in range\n"
					getPFPages site pc pr bd rh cookies results
		_ -> do
			maybeClose rh
			hPutStr stderr "propertyfinder: no properties in range\n"

getPFPages :: String -> Postcode -> PriceRange -> BedsRange -> Maybe Handle -> Cookies -> DOM -> IO ()
getPFPages site pc pr bd h cookies d = do
	ls <- return $ getPFLinks site "get" d
	print ls
	getPFProperties pc pr bd d
	maybeClose h
	getPFNext ls pc pr bd cookies

getPFNext :: [URL] -> Postcode -> PriceRange -> BedsRange -> Cookies -> IO ()
getPFNext [] _ _ _ _ = return ()
getPFNext (u0:us) pc pr bd cookies = do
	(sh,sub) <- getDocCook (MkElem (Document,u0,[])) cookies
	getPFProperties pc pr bd sub
	maybeClose sh
	getPFNext us pc pr bd cookies

getPFProperties :: Postcode -> PriceRange -> BedsRange -> DOM -> IO ()
getPFProperties pc pr bd d = case filterDOM (do
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [3]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
 		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [3]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		discardBranches 1) d of
	DOMOut _ d' -> getPFLoop pc pr bd d'
	_ -> return ()

getPFLoop :: Postcode -> PriceRange -> BedsRange -> DOM -> IO ()
getPFLoop pc pr bd d = case filterDOM ( do
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	pr <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
	bd <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	(from . elemIs) (MkElem (Tag,"TD",[])) -- link
	dc <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	ad <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
	ag <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	return (extractPrice pr,extractString bd,extractString dc,extractString ad,extractString ag)) d of
	DOMOut p d' -> do
		-- hPutStr stderr $ showString (generateXML d') "\n"
		print p
		getPFLoop pc pr bd d'
	-- DOMOut _ d' -> return () -- getPFLoop pc pr bd d'
	_ -> return ()

getPFLinks :: String -> String -> DOM -> [String]
getPFLinks site m d = case filterDOM (do
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [2]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TR",[]))
		select [2]
		cut 1
		(graft . elemIs) (MkElem (Tag,"A",[]))
		l <- filterLinks
		return l) d of
	DOMOut l _ -> map (linkToURL site m) l
	_ -> []

getPFInRange :: (URL,Cookies,FormVars) -> Postcode -> Maybe BedsRange -> Maybe PriceRange
	-> IO (Maybe Handle,DOM)
getPFInRange (action,cookies,vars) pc bedsRange priceRange = case priceRange of
	Just (minPrice,maxPrice) -> case bedsRange of
		Just (minBeds,maxBeds) -> do
			(h,src) <- getDocCook (MkElem (Document,action,setInputs [
				MkAttribute ("Minprice",priceToString minPrice),
				MkAttribute ("Maxprice",priceToString maxPrice),
				MkAttribute ("Minbedrooms",bedsToString minBeds),
				MkAttribute ("Maxbedrooms",bedsToString maxBeds),
				MkAttribute ("postcode",pc),
				MkAttribute ("sorting","down")
				] vars)) cookies
			hPutStr stderr $ showString (generateXML src) "\n"
			return (h,fTidyDOM src)
		Nothing -> return (Nothing,[])
	Nothing -> return (Nothing,[])

gotAllPFProperties :: DOM -> Maybe Bool
gotAllPFProperties d = case filterDOM (do
		(graft . elemIs) (MkElem (Tag,"TABLE",[]))
		select [2]
		cut 1
		(graft . elemIs) (MkElem (Tag,"TR",[]))
		select [1]
		cut 1) d of
	DOMOut _ d' -> Just (case parseResult (stringNcs "more than") (extractString d') of
		Ok _ _ -> False
		_ -> True)
	_ -> Nothing

