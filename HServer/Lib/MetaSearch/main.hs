-- main.hs (C)2001 Keean Schupke
--
--		main program, uses DOM module 

module Main(main) where
import GHC.IOBase (unsafeInterleaveIO)
import Char
import IO
import Lib.MetaSearch.Parser
import Lib.MetaSearch.DOM
import Lib.MetaSearch.Filter
import Lib.MetaSearch.Forest

------------------------------------------------------------------------------
-- Restaurant datatype

data Restaurant = Restaurant {
	restaurantName :: Maybe String,
	restaurantType :: Maybe String,
	restaurantAddress :: Maybe String,
	restaurantPhone :: Maybe String,
	restaurantMapRef :: Maybe String}

makeRestaurant :: Restaurant
makeRestaurant = Restaurant {
	restaurantName = Nothing,
	restaurantType = Nothing,
	restaurantAddress = Nothing,
	restaurantPhone = Nothing,
	restaurantMapRef = Nothing}

showRestaurant :: Restaurant -> ShowS
showRestaurant r = (case restaurantName r of
	Just n -> showString (toCSV True n)
	Nothing -> id) . showString "," . (case restaurantAddress r of
	Just a -> showString (toCSV False a)
	Nothing -> id) where -- . showString "| " . (case restaurantType r of
	-- Just t -> showString (toCSV t)
	-- Nothing -> id) . showString "| " . (case restaurantPhone r of
	-- Just p -> showString (toCSV p)
	-- Nothing -> id) . showString "| " . (case restaurantMapRef r of
	-- Just m -> showString (toCSV m)
	-- Nothing -> id) where

		toCSV :: Bool -> String -> String
		toCSV b (c0:cs) = case c0 of
			c | (not $ isPrint c) -> toCSV b cs
			  | (c == ',') && b -> ';':toCSV b cs
			  | (c == '.') -> ':':toCSV b cs
			_ -> c0:toCSV b cs
		toCSV _ _ = []

showRestaurantList :: [Restaurant] -> ShowS
showRestaurantList (r0:rs) = case rs of
	(_:_) -> showRestaurant r0 . showString ".\r\n" . showRestaurantList rs
	_ -> showRestaurant r0
showRestaurantList _ = id

instance Show Restaurant where
	showsPrec _ r = showRestaurant r
	showList rs = showRestaurantList rs

------------------------------------------------------------------------------
-- extraction code

splitBracket :: String -> (String,String)
splitBracket s = (outside 0 s,inside 0 s) where

	outside :: Int -> String -> String
	outside i (c0:cs) = case c0 of
		c | (c=='(') -> outside (i+1) cs 
		  | (c==')') -> outside (i-1) cs
		  | otherwise -> (if i<1 then c:outside i cs else outside i cs)
	outside _ _ = []
		
	inside :: Int -> String -> String
	inside i (c0:cs) = case c0 of
		c | (c=='(') -> inside (i+1) cs 
		  | (c==')') -> inside (i-1) cs
		  | otherwise -> (if i>0 then c:inside i cs else inside i cs)
	inside _ _ = []
		
filterRestaurant :: FilterDOM Restaurant
filterRestaurant = do
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	(from . elemIs) (MkElem (Tag,"TR",[]))
	rest <- (moveInc . elemIs) (MkElem (Tag,"TR",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	addb <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
	(from . elemIs) (MkElem (Tag,"TR",[]))
	(from . elemIs) (MkElem (Tag,"TD",[]))
	phone <- maybeFilter ((moveInc . elemIs) (MkElem (Tag,"TD",[])))
	mapRef <- maybeFilter ((moveExc . elemIs) (MkElem (Tag,"TABLE",[])))
	(name,ty) <- return $! (splitBracket . extractString) rest
	addr <- return $! extractString addb
	if name == "" || addr == "" then mzero else return $! makeRestaurant {
		restaurantName = Just name,
		restaurantType = Just ty,
		restaurantAddress = Just addr,
		restaurantPhone = fmap extractString phone,
		restaurantMapRef = case mapRef of
			Just m -> getLink "href" m
			_ -> Nothing}

filterRestaurantList :: FilterDOM [Restaurant]
filterRestaurantList = do
	r0 <- filterRestaurant 
	rs <- filterRestaurantList `mplus` (return [])
	return $! r0:rs

filterResults :: FilterDOM [Restaurant]
filterResults = do
	alter tidyDOM 
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	select [0]
	cut 1
	(graft . elemIs) (MkElem (Tag,"TABLE",[]))
	discardBranches 3
	r <- filterRestaurantList
	return $! r

filterNextLink :: FilterDOM (Maybe String)
filterNextLink = do
	(from . elemIs) (MkElem (Tag,"TABLE",[]))
	l <- (moveInc . elemIs) (MkElem (Tag,"TABLE",[]))
	return $! fmap (linkToURL "//www.restaurants.co.uk" "get") (getLinkWithText "|Next >" "href" l)

resultLoop :: Elem -> IO [Restaurant]
resultLoop e = do
	(h,src) <- getDocument e
	case filterDOM filterResults src of
		DOMOut r5 ls -> case filterDOM filterNextLink ls of
			DOMOut (Just l) _ -> do
				hPutStr stderr "."
				maybeClose h
				rs <- unsafeInterleaveIO $ resultLoop (MkElem (Document,l,[]))
				return $ r5++rs
			_ -> do
				maybeClose h
				return r5
		_ -> do
			maybeClose h
			return []

------------------------------------------------------------------------------
-- do the search

main :: IO ()
main = do
	rs <- resultLoop (MkElem (Document,"http-post://www.restaurants.co.uk/rest_includes/search.php",[
		MkAttribute ("search1",""),
		MkAttribute ("search2","London"),
		MkAttribute ("pcode",""),
		MkAttribute ("search3",""),
		MkAttribute ("search4","Indian")]))
	putStr (showRestaurantList rs "\r\n")
	hPutStr stderr "\n"

