{-# LANGUAGE ScopedTypeVariables #-}

--      main program, uses DOM module 

module Lib.MetaSearch.MetaSearch
    {-(metaSearch,Property(..),searchPrimelocation,searchRightmove,
    searchPropertyfinder,searchPureproperty)-} where
import Control.Concurrent 
import Control.DeepSeq
import Data.Char
--import qualified Data.Int as Int
import System.IO
import Lib.MetaSearch.Parser
import Lib.MetaSearch.DOM
import Lib.MetaSearch.Filter
import Lib.MetaSearch.Forest
import Lib.MetaSearch.Forms
import Lib.MetaSearch.Cookies 
--import qualified Data.Map as Map
--import Lib.MetaSearch.Shuffle
import Lib.MetaSearch.Redirect
import Numeric
import Control.Exception as Exception
import Control.Parallel.Strategies hiding (r0)
import Data.Time.Clock
import System.Random

------------------------------------------------------------------------------
-- data types for searching


type Site = String
type Postcode = String
type PCodeList = String
type Key = String
type Agent = String
type Price = Int
type Link = String
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
type Image = String
type RangeCode = ([(Int,String)],[(Int,String)],[(Int,String)],[(Int,String)])

data Property = Property {
    propertyTimestamp :: Maybe UTCTime,
    propertySite :: Maybe Site,
    propertyPostcode :: Maybe Postcode,
    propertyPrice :: Maybe Price,
    propertyBeds :: Maybe Beds,
    propertyType :: Maybe Type,
    propertyAddress :: Maybe Address,
    propertyDescription :: Maybe Description,
    propertyAgent :: Maybe Agent,
    propertyAgentLink :: Maybe Link,
    propertyImage :: Maybe Image,
    propertyDetails :: Maybe Link} deriving Show

------------------------------------------------------------------------------
-- Strategies:
--      Strategies are used to force strict evaluation of results before files
--  are closed, and to force strict evaluation of results before they are pushed
-- into the mergeChannel

{-
instance NFData a => NFData (Maybe a) where
    rnf Nothing = () 
    rnf (Just x) = rnf x `seq` () 
-}

instance NFData Property where
    rnf (Property a b c d e f g h i j k l) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
        `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j `seq` rnf k `seq` rnf l `seq` () 

newtype Ranges a b c d e f g h i j k = Ranges (a, b, c, d, e, f, g, h, i, j, k)

instance (NFData a,NFData b,NFData c,NFData d,NFData e,NFData f,NFData g,NFData h,NFData i,NFData j,NFData k) =>
        NFData (Ranges a b c d e f g h i j k) where
    rnf (Ranges (a,b,c,d,e,f,g,h,i,j,k)) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d
        `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h `seq` rnf i `seq` rnf j `seq` rnf k `seq` ()
{-
instance NFData Attribute where
    rnf (MkAttribute (x,y)) = rnf x `seq` rnf y `seq` ()

instance NFData ElemType where
    rnf t = t `seq` ()

instance NFData UTCTime where
    rnf t = t `seq` ()

instance NFData Elem where
    rnf (MkElem (t,n,a)) = rnf t `seq` rnf n `seq` rnf a `seq` ()
-}

------------------------------------------------------------------------------
-- merge results code

parsePostcodes :: Parser [Postcode]
parsePostcodes = do
    pc <- many $ do
        _ <- untilChar isAlpha
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

metaSearch :: String -> Int -> Int -> Int -> Int -> IO ([Property],[ThreadId])
metaSearch as pr0 pr1 bd0 bd1 = do
    case parseResult parsePostcodes (map toUpper as) of
        Ok pc _ -> do
            hPutStr stderr $ (showChar '[' . showString (pcodeList pc) . showChar ',' . showInt pr0 . showChar '-'
                . showInt pr1 . showChar ',' . showInt bd0 . showChar '-' . showInt bd1) "]\n"
            mergeChannel <- newChan
            results <- getChanContents mergeChannel
            {- no longer exists
            t1 <- forkIO $
                (searchPureproperty "www.pureproperty.com" mergeChannel pc (pr0,pr1) (bd0,bd1))
                `Exception.catch` (\e -> hPutStr stderr $ (showString "pureproperty: " . shows e) "\n")
                `finally` (do 
                    hPutStr stderr "pureproperty: finally\n"
                    writeChan mergeChannel Nothing)
            -}
            {- disbled not working
            t2 <- forkIO $
                (searchPrimelocation mergeChannel pc (pr0,pr1) (bd0,bd1))
                `Exception.catch` (\e -> hPutStr stderr $ (showString "primelocation: " . shows e) "\n")
                `finally` (do
                    hPutStr stderr "primelocation: finally\n"
                    writeChan mergeChannel Nothing)
            -}
            {- disabled not working
            t3 <- forkIO $
                (searchRightmove "www.rightmove.co.uk" mergeChannel pc (pr0,pr1) (bd0,bd1))
                `Exception.catch` (\e -> hPutStr stderr $ (showString "rightmove: " . shows e) "\n")
                `finally` (do
                    hPutStr stderr "rightmove: finally\n"
                    writeChan mergeChannel Nothing)
            -}
            t4 <- forkIO $
                (searchPropertyfinder "www.propertyfinder.co.uk" mergeChannel pc (pr0,pr1) (bd0,bd1))
                `Exception.catch` (\(e::SomeException) -> hPutStr stderr $ (showString "propertyfinder: " . shows e) "\n")
                `finally` (do
                    hPutStr stderr "propertyfinder: finally\n"
                    writeChan mergeChannel Nothing)
            return (findEnd 1 results,[t4])
        _ -> return ([],[])

------------------------------------------------------------------------------
-- shared definitions

makeProperty :: Property
makeProperty = Property {
        propertyTimestamp = Nothing,
        propertySite = Nothing,
        propertyPostcode = Nothing,
        propertyPrice = Nothing,
        propertyBeds = Nothing,
        propertyType = Nothing,
        propertyAddress = Nothing,
        propertyDescription = Nothing,
        propertyAgent = Nothing,
        propertyAgentLink = Nothing,
        propertyImage = Nothing,
        propertyDetails = Nothing}

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
    join _ = ""

joinLists :: [a] -> [a] -> [a]
joinLists [] b = b
joinLists (a0:as) b = a0:joinLists as b

splitRange :: [a] -> Maybe ([a],[a])
-- splitRange l = splitRange' 1 (floor ((toRational (length l))/(toRational 2.0))) l [] where
splitRange [] = Nothing
splitRange (_:[]) = Nothing
splitRange l = Just $ splitRange' 1 ((length l) `div` 2) l [] where

    splitRange' :: Int -> Int -> [a] -> [a] -> ([a],[a])
    splitRange' _ _ [] r = (reverse r,[])
    splitRange' i j (l0:ls) r = if i<j
        then splitRange' (i+1) j ls (l0:r)
        else (reverse (l0:r),ls)

mixRange :: [(a,a)] -> Maybe (a,a)
mixRange u = case unzip u of
    ([],_) -> Nothing
    ((l0:_),r) -> case reverse r of
        [] -> Nothing
        (r0:_) -> Just (l0,r0)

pickRange :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
pickRange _ [] = []
pickRange (rmin,rmax) [(i,j)]
    | i<=rmin && j>=rmax = [(i,j)]
    | otherwise = []
pickRange (rmin,rmax) ((p0,q0):rs@((p1,_):_)) = if (if p1 < 0 then 0 else p1) <= rmin
    then pickRange (rmin,rmax) rs
    else let q' = if q0 <= 0 then (maxBound :: Int) else q0
        in (if p0 < 0 then 0 else p0,q'):if rmax > q' then pickMax rmax rs else []

pickMax :: Int -> [(Int,Int)] -> [(Int,Int)]
pickMax _ [] = []
pickMax rmax (_r0@(p0,q0):rs) = let q' = if q0 <= 0 then (maxBound :: Int) else q0
    in if rmax > q'
        then (if p0 < 0 then 0 else p0,q'):pickMax rmax rs
        else [(if p0 < 0 then 0 else p0,q')]

checkRange :: Chan (Maybe Property) -> (Int,Int) -> (Int,Int) -> Property -> IO ()
checkRange mergeChannel _pr@(pr0,pr1) _bd@(bd0,bd1) p0 = case (propertyPrice p0,propertyBeds p0) of
    (Just p,Just b) -> if (pr0 <= p) && (pr1 >= p) && (bd0 <= b) && (bd1 >= b)
        then do
            t <- getCurrentTime
            writeChan mergeChannel ((Just (p0 {propertyTimestamp = Just t})) `using` rdeepseq)
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
    | j0<=0 = (maxBound :: Int):clipMax js
    | otherwise = j0:clipMax js

findMatch :: [Int] -> [Int] -> Maybe Int
findMatch x y = fm x y where
    
    fm :: [Int] -> [Int] -> Maybe Int
    fm (i0:[]) [] = Just i0
    fm (_i0:is@(_:_)) [] = fm is y
    fm i@(i0:_is) (j0:js)
        | i0 == j0 = Just i0
        | otherwise = fm i js
    fm _ _ = Nothing

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

getAgents :: PCodeList -> IO [Agent]
getAgents pcds = do
    (h,src) <- getDocument (MkElem (Document,"http-get://www.primelocation.com/agentSearch/agentSearch.cfm",[
        MkAttribute ("whichsearch","all"), MkAttribute ("searchClass","agent"),
        MkAttribute ("location",pcds)]))
    (case filterDOM (do
            alter tidyDOM
            processForm
            _ <- matchForm "ladder"
            lst <- matchSelect getOptionsText "lst"
            rnf lst `seq` return lst) src of
        DOMOut agents _ -> do
            ags <- return (map filterAgent agents)
            rnf ags `seq` maybeClose h
            return ags
        _ -> do
            maybeClose h
            return []) `Exception.catch` (\(e::SomeException) -> do maybeClose h; throw e)
    where

    filterAgent :: String -> Agent
    filterAgent as = case parseResult ( do
        _ <- untilChar (==':')
        _ <- (skipSpace . whileChar) (==':')
        b <- untilParserOrEnd (string " - " `mplus` string ", ") char
        return b) as of
            Ok r _ -> reverse r
            _ -> reverse as

getKey :: IO (URL,Cookies,FormVars)
getKey = do 
    (h,src) <- getDocument (MkElem (Document,"http-get://www.primelocation.com/",[]))
    (case filterDOM (do
            alter tidyDOM
            c <- getCookies
            (_,a) <- matchForm "searchForm"
            processForm
            f <- getInputs
            rnf (a,c,f) `seq` return (a,c,f)) src of
        DOMOut acf _ -> do
            rnf acf `seq` maybeClose h 
            return acf
        _ -> do
            maybeClose h
            return ([],[],[])) `Exception.catch` (\(e::SomeException) -> do maybeClose h; throw e)

getRanges :: IO (URL,Cookies,FormVars,[PriceRange],[BedsRange],[(Int,String)],[(Int,String)],[(Int,String)],[(Int,String)])
getRanges = do
    (h,src) <- getDocument (MkElem (Document,"http-get://www.primelocation.com/",[]))
    (case filterDOM (do 
            alter tidyDOM
            c <- getCookies
            (_,a) <- matchForm "searchForm"
            processForm
            f <- getInputs
            minprice <- matchSelect getOptionsText "minprice" 
            minprice' <- matchSelect getOptions "minprice"
            maxprice <- matchSelect getOptionsText "maxprice"
            maxprice' <- matchSelect getOptions "maxprice"
            minbeds <- matchSelect getOptionsText "minbedrooms"
            minbeds' <- matchSelect getOptions "minbedrooms"
            maxbeds <- matchSelect getOptionsText "maxbedrooms"
            maxbeds' <- matchSelect getOptions "maxbedrooms"
            rnf (Ranges (a,c,f,minprice,maxprice,minbeds,maxbeds,minprice',maxprice',minbeds',maxbeds')) `seq` return
                (a,c,f,makeRanges (getInts minprice) (getInts maxprice),makeRanges (getInts minbeds) (getInts maxbeds),
                zip (getInts minprice) minprice',zip (getInts maxprice) maxprice',
                zip (getInts minbeds) minbeds',zip (getInts maxbeds) maxbeds')) src of
        DOMOut rtn _ -> do
            rnf rtn `seq` maybeClose h
            return rtn 
        _ -> do
            maybeClose h
            return ([],[],[],[],[],[],[],[],[])) `Exception.catch` (\(e::SomeException) -> do maybeClose h; throw e)

getInRange :: URL -> [Attribute] -> Maybe PriceRange -> Maybe BedsRange -> Postcode -> Cookies -> RangeCode -> IO (Maybe Handle,DOM)
getInRange url key priceRange bedsRange postcode cookies (mipren,mapren,mibden,mabden) = case priceRange of 
    Just (minPrice,maxPrice) -> case bedsRange of
        Just (minBeds,maxBeds) -> do
            -- hPutStr stderr $ (showChar '<' . showInt minBeds . showChar '-' . showInt maxBeds . showChar ',' .
            --  showInt minPrice . showChar '-' . showInt maxPrice) ">"
            -- src <- getDocument (MkElem (Document,"file:test.html",[]))
            x0 <- rand (0,23)
            y0 <- rand (0,23)
            doc <- return $ MkElem (Document,"http-get://www.primelocation.com/"++url,setInputs [
                MkAttribute ("place",postcode), MkAttribute ("maxprice",encode mapren maxPrice),
                MkAttribute ("minprice",encode mipren minPrice), MkAttribute ("minbedrooms",encode mibden minBeds),
                MkAttribute ("maxbedrooms",encode mabden maxBeds), MkAttribute ("propertyType","ALL"),
                MkAttribute ("x",showInt x0 ""),MkAttribute("y",showInt y0 "")] key)
            -- hPutStr stderr $ (shows doc "\n")
            -- hPutStr stderr $ (shows cookies "\n")
            hsrc <- getDocCook doc cookies
            rdr <- followRedirects "http-get://www.primelocation.com" hsrc cookies
            return rdr 
        Nothing -> return (Nothing,[])
    Nothing -> return (Nothing,[])

rand :: (Int,Int) -> IO Int
rand ij = getStdRandom (randomR ij)

encode :: [(Int,String)] -> Int -> String
encode [] _ = ""
encode ((i0,s0):iss) i
    | i == i0 = s0
    | otherwise = encode iss i

pricePLString :: Int -> String
pricePLString i 
    | i>15000000 = "0"
    | i<=0 = "0"
    | otherwise = showInt i ""

bedsPLString :: Int -> String
bedsPLString i
    | i>9 = "0"
    | i<=0 = "0"
    | otherwise = showInt i ""

parseInt :: Parser String
parseInt = do
    p <- (required . whileChar) isDigit
    _ <- whileChar (==',')
    ps <- parseInt `mplus` return ""
    return (p++ps)

parseAgents :: [Agent] -> Parser Agent
parseAgents ps = Parser (\cs -> parse (tryAgents ps) (reverse cs)) where

    tryAgents :: [Agent] -> Parser Agent
    tryAgents [] = Parser (\_ -> Empty Error)
    tryAgents (p:ps') = Parser (\cs -> case parse (untilParser (stringNcs p) char) cs of
        Consumed (Ok m r) -> Consumed $ Ok (reverse (m++p)) (reverse r)
        Empty (Ok _ r) -> Consumed $ Ok (reverse p) (reverse r)
        _ -> parse (tryAgents ps') cs)

parseProperty :: [Agent] -> Postcode -> Parser Property
parseProperty agents pc = do
    _ <- untilParser (stringNcs "&pound;") char
    price <- (skipSpace . untilChar) isSpace
    bedrooms <- (do
        beds <- (skipSpace . whileChar) isDigit
        skipSpace $ do
            stringNcs "bedroom"
            ((do _ <- satisfy (=='s');return ()) `mplus` return ())
        return beds) `mplus` (return "0")

    addr <- (skipSpace . untilParserOrEnd (string "..")) char
    agent <- (do
        string ".."
        _ <- (skipSpace . whileChar) isSpace
        untilEnd) `mplus` (return "")
    (ad,ag) <- return $ getAddress agents addr agent
    return $ makeProperty {
        propertySite = Just "PL",
        propertyPostcode = Just pc,
        propertyPrice = case getPrice price of
            p   | p >= 0 -> Just p
                | otherwise -> Nothing,
        propertyBeds = case read bedrooms of
            b   | b >= 0 -> Just b
                | otherwise -> Nothing,
        propertyAddress = Just ad,
        propertyAgent = Just ag} where
        
        getPrice :: String -> Price
        getPrice p = case parseResult parseInt p of
            Ok p' _ -> read p'
            _ -> -1

        getAddress :: [Agent] -> Address -> Agent -> (Address,Agent)
        getAddress agents' addr agent = if agent /= ""
            then (addr,agent)
            else case parseResult (parseAgents agents') addr of
                Ok a b -> case parse (untilParser (stringNcs pc) char) b of
                    Consumed (Ok x _) -> (x,a)
                    _ -> (b,a)
                _ -> case parse (untilParser (stringNcs pc) char) addr of
                    Consumed (Ok x y) -> (x,y)
                    Empty (Ok _ y) -> ("",y)
                    _ -> (addr,agent) 

-- recursive split if results full
getPostcode :: Chan (Maybe Property) -> (Int,Int) -> (Int,Int) -> [PriceRange] -> [BedsRange] -> Postcode -> [Agent]
    -> RangeCode -> (URL,Cookies,FormVars) -> IO ()
getPostcode mergeChannel pr bd prices beds pcode agents rcode acf@(url,cookies,key) = do
    (h,src) <- getInRange url key (mixRange prices) (mixRange beds) pcode cookies rcode
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
            -- hPutStr stderr $  showString (generateXML frm) "\n"
            str <- return $ extractString frm
            case parseResult parseComplete str of
                Ok _ _ -> do
                    -- hPutStr stderr "."
                    -- extractProperties mergeChannel pr bd pcode frm agents
                    getPLProperties h mergeChannel pr bd pcode frm cookies
                _ -> case parseResult parseIncomplete str of 
                    Ok _ _ -> case splitRange prices of
                        Just (p0,p1) -> do
                            -- hPutStr stderr "("
                            maybeClose h
                            case p0 of
                                [] -> return ()
                                _ -> getPostcode mergeChannel pr bd p0 beds pcode agents rcode acf
                            case p1 of
                                [] -> return ()
                                _ -> getPostcode mergeChannel pr bd p1 beds pcode agents rcode acf
                            -- hPutStr stderr ")"
                        _ -> case splitRange beds of
                            Just (b0,b1) -> do
                                -- hPutStr stderr "["
                                maybeClose h
                                case b0 of
                                    [] -> return ()
                                    _ -> getPostcode mergeChannel pr bd prices b0 pcode agents rcode acf
                                case b1 of
                                    [] -> return ()
                                    _ -> getPostcode mergeChannel pr bd prices b1 pcode agents rcode acf
                                -- hPutStr stderr "]"
                            _ -> do
                                -- hPutStr stderr "!"
                                -- extractProperties mergeChannel pr bd pcode frm agents
                                getPLProperties h mergeChannel pr bd pcode frm cookies
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
        _ <- untilChar (=='(')
        _ <- (required . whileChar) (=='(')
        stringNcs "complete)"
        return ()

    parseIncomplete :: Parser ()
    parseIncomplete = do
        _ <- untilChar (=='(')
        _ <- (required . whileChar) (=='(')
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
                DOMOut a _d' -> do
                    getProperty mergeChannel pr bd pcode agents (extractString a)
                    extractProperty mergeChannel pr bd pcode ds agents i
                _ -> extractProperty mergeChannel pr bd pcode ds agents i
        _ -> extractProperty mergeChannel pr bd pcode ds agents i
    | otherwise = return ()

getPLLinks :: [String] -> [String]
getPLLinks [] = []
getPLLinks (l:ls) = case parseResult (do
        stringNcs "javascript:goTo('"
        a <- untilParser (stringNcs "')") char
        return a) l of
    Ok link _ -> ("http-get://www.primelocation.com"++link):getPLLinks ls
    _ -> getPLLinks ls

getPLProperties :: Maybe Handle -> Chan (Maybe Property) -> PriceRange -> BedsRange -> Postcode
    -> DOM -> Cookies -> IO ()
getPLProperties h mergeChannel pr bd pcode d cookies = case filterDOM (do
        -- (graft . elemIs) (MkElem (Tag,"TABLE",[]))
        (from . elemIs) (MkElem (Tag,"SPAN",[MkAttribute ("class","link3")]))
        l <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
        return l) d of
    DOMOut l d' -> do
        -- hPutStr stderr $ showString (generateXML d) "\n"
        links <- return $ getPLLinks (getLinks l) 
        hPutStr stderr $ shows links "\n"
        -- hPutStr stderr $ showString (generateXML d') "\n"
        getPLLoop mergeChannel pcode pr bd d'
        maybeClose h
        getPLNext mergeChannel pcode pr bd links cookies
    _ -> do
        maybeClose h
        return ()

getPLNext :: Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> [String] -> Cookies -> IO ()
getPLNext _mergeChannel _ _ _ [] _ = return ()
getPLNext mergeChannel pcode pr bd (p0:ps) cookies = do
    (h,src) <- getDocCook (MkElem (Document,p0,[])) cookies
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
            cut 1
            (from . elemIs) (MkElem (Tag,"SPAN",[MkAttribute ("class","link3")]))
            (from . elemIs) (MkElem (Tag,"TD",[]))) src of
        DOMOut _ page -> do
            getPLLoop mergeChannel pcode pr bd page
            maybeClose h
            getPLNext mergeChannel pcode pr bd ps cookies
        _ -> do
            maybeClose h
            getPLNext mergeChannel pcode pr bd ps cookies
    
plPropertyFilter :: Postcode -> FilterDOM Property
plPropertyFilter pc = do
    (from . elemIs) (MkElem (Tag,"TABLE",[]))
    (from . elemIs) (MkElem (Tag,"TABLE",[]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    ad <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    im <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"SPAN",[]))
    pr <- (moveInc . elemIs) (MkElem (Tag,"SPAN",[]))
    bd <- (moveInc . elemIs) (MkElem (Tag,"SPAN",[]))
    dc <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"TABLE",[]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    ag <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"TABLE",[]))
    return $! makeProperty { 
        propertySite = Just "PL",
        propertyPostcode = Just pc,
        propertyAddress = extractLiteral ad,
        propertyPrice = extractPrice pr,
        propertyBeds = extractBeds bd,
        propertyDescription = extractLiteral dc,
        propertyAgent = fmap removeBlurb (getImage "alt" ag),
        propertyImage = getImage "src" im}

getPLLoop :: Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> DOM -> IO ()
getPLLoop mergeChannel pc pr bd d = case filterDOM (plPropertyFilter pc) d of
    DOMOut p d' -> do
        checkRange mergeChannel pr bd p
        getPLLoop mergeChannel pc pr bd d'
    _ -> return ()

removeBlurb :: String -> String
removeBlurb s = case parseResult (stringNcs "click here to contact ") s of
    Ok _ r -> r
    _ -> s
    
getImage :: String -> DOM -> Maybe String
getImage _ [] = Nothing
getImage t ((_i,e):d) = case e of
    (MkElem (Tag,"IMG",as)) -> Just $ getAttributeAsString t as
    _ -> getImage t d

getProperty :: Chan (Maybe Property) -> PriceRange -> BedsRange -> Postcode
    -> [Agent] -> String -> IO ()
getProperty mergeChannel pr bd pc agents p = case parseResult (parseProperty agents pc) p of
    Ok pp _ -> checkRange mergeChannel pr bd pp
    _ -> return ()

getProperties :: Chan (Maybe Property) -> (Int,Int) -> (Int,Int) -> [PriceRange] -> [BedsRange] -> [Postcode] -> [Agent] -> RangeCode -> (URL,Cookies,FormVars) -> IO ()
getProperties _ _ _ [] _ _ _ _ _ = return ()
getProperties _ _ _ _ [] _ _ _ _ = return ()
getProperties _ _ _ _ _ [] _ _ _ = return ()
getProperties mergeChannel pr bd prices beds _postcodes@(p0:ps) agents rcode acf = do
    -- hPutStr stderr ("{"++p0)
    getPostcode mergeChannel pr bd prices beds p0 agents rcode acf
    -- hPutChar stderr '}'
    getProperties mergeChannel pr bd prices beds ps agents rcode acf

searchPrimelocation :: Chan (Maybe Property) -> [Postcode] -> (Int,Int) -> (Int,Int) -> IO ()
searchPrimelocation mergeChannel pcodes pr bd = do
    hPutStr stderr $ "primelocation: start\n"
    -- (h,src) <- getDocument (MkElem (Document,"http-get://www.primelocation.com/",[]))
    -- print src
    -- hPutStr stderr $ showString (generateXML src) "\n"
    (url,cookies,key,prices,beds,mipren,mapren,mibden,mabden) <- getRanges
    hPutStr stderr $ (showString "PL: "
        . shows prices
        . showString "\n"
        . shows beds
        . showString "\n"
        . shows mipren
        . showString "\n"
        . shows mapren
        . showString "\n"
        . shows mibden
        . showString "\n"
        . shows mabden) "\n"
    -- agents <- getAgents (pcodeList pcodes)
    hPutStr stderr $ (showString "PL: " . shows (pickRange pr prices) . shows (pickRange bd beds)) "\n"
    -- getProperties mergeChannel pr bd (pickRange pr prices) (pickRange bd beds) pcodes agents
    getProperties mergeChannel pr bd (pickRange pr prices) (pickRange bd beds) pcodes [] (mipren,mapren,mibden,mabden) (url,cookies,key)

--------------------------------------------------------------------
-- search code for rightmove

searchRightmove :: String -> Chan (Maybe Property) -> [Postcode] -> PriceRange -> BedsRange -> IO ()
searchRightmove site mergeChannel pc pr bd = do
    form1 <- getRMForm1 site
    getRMPostcodes mergeChannel site form1 pc pr bd

getForm :: String -> String -> DOM -> (DOM,(URL,Cookies,FormVars))
getForm site _ [] = ([],("http://"++site++"/",[],[]))
getForm site formName d = case filterDOM (do
        alter tidyDOM
        c <- getCookies
        (m,a) <- matchForm formName
        rnf (m,a,c) `seq` return (m,a,c)) d of
    DOMOut (m,a,c) f -> case filterDOM (do
            processForm
            v <- getInputs
            rnf (m,a,c,v) `seq` return (linkToURL site m a,c,v)) d of
        DOMOut acv _ -> (f,acv)
        DOMVoid -> (f,("http://"++site++"/",[],[]))
    _ -> ([],("http://"++site++"/",[],[]))

getRMForm1 :: String -> IO (URL,Cookies,FormVars)
getRMForm1 site = do
    (_,h,src) <- getPageWithRedirects [] [] ("http://"++site++"/")
    (do
        (_,form) <- return $ getForm site "property_search_form" src
        rnf form `seq` maybeClose h
        return form) `Exception.catch` (\(e::SomeException) -> do maybeClose h; throw e) 

getRMForm2 :: String -> (URL,Cookies,FormVars) -> Postcode -> IO ((URL,Cookies,FormVars),BedsRanges,PriceRanges)
getRMForm2 site (action,cookies,vars) pc = do
    (h,src) <- getDocCook (MkElem (Document,action,setInputs [MkAttribute ("s_lo",pc)] vars)) cookies
    (do
        (rst,(a2,c2,v2)) <- return $ getForm site "modify_search_criteria_form" src
        beds <- return $ zip (map read $ domToSelect getOptions "mi_b" rst) zeroList
        price <- return $ zip (map read $ domToSelect getOptions "mi_p" rst)
            (map read $ rolList $ domToSelect getOptions "ma_p" rst)
        rnf (a2,c2,v2,beds,price) `seq` maybeClose h
        return ((a2,setInputs c2 cookies,v2),beds,price)) `Exception.catch` (\(e::SomeException) -> do maybeClose h; throw e)
            
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

getRMNext :: String -> Chan (Maybe Property) -> DOM -> Postcode -> PriceRange -> BedsRange -> IO (Maybe URL)
getRMNext site mergeChannel d pc pr bd = case filterDOM ( do
        (graft . elemIs) (MkElem (Tag,"TABLE",[]))
        select [2]
        cut 1
        (from . elemIs) (MkElem (Tag,"IMG",[MkAttribute ("name","previous_page")]))
        xa <- (moveExc .elemIs) (MkElem (Tag,"IMG",[]))
        (graft . elemIs) (MkElem (Tag,"FORM",[MkAttribute ("name","add_to_shortlist")]))
        cut 1
        (graft . elemIs) (MkElem (Tag,"TABLE",[]))
        x <- rnf xa `seq` return $ getAttributeAsString "href" $ case xa of
            ((_,MkElem (_,_,a)):_) -> a
            _ -> []
        rnf x `seq` return x) d of
    DOMOut url rst -> do
        -- hPutStr stderr $ showString (generateXML d) "\n"
        hPutStr stderr $ "rightmove: nextpage ("++url++")\n"
        getRMProperties site mergeChannel pc pr bd rst
        return (Just url)
    _ -> do
        -- hPutStr stderr $ showString (generateXML d) "\n"
        hPutStr stderr "rightmove: no more.\n"
        getRMProperties site mergeChannel pc pr bd d
        return Nothing

getRMPages :: String -> Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> Maybe Handle -> Cookies -> DOM -> IO ()
getRMPages site mergeChannel pc pr bd h cookies d = do
    u <- getRMNext site mergeChannel d pc pr bd
    case u of
        Nothing -> maybeClose h
        Just url -> do
            rnf url `seq` maybeClose h
            -- hPutStr stderr $ (showString "[" . shows cookies) "]\n"
            (nh,np) <- getDocCook (MkElem (Document,url,[])) cookies
            getRMPages site mergeChannel pc pr bd nh cookies (fTidyDOM np)

getRMProperties :: String -> Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> DOM -> IO ()
getRMProperties site mergeChannel pc pr bd d = case filterDOM (rmPropertyFilter site pc) d of
    DOMOut m d' -> do
        hPutStr stderr $ (showString "RM:" . shows m) "\n"
        checkRange mergeChannel pr bd m
        getRMProperties site mergeChannel pc pr bd d'
    _ -> return ()

rmPropertyFilter :: String -> Postcode -> FilterDOM Property
rmPropertyFilter site pc = do
    (from . elemIs) (MkElem (Tag,"TABLE",[]))
    (from . elemIs) (MkElem (Tag,"A",[]))
    im <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"P",[]))
    pr <- (moveInc . elemIs) (MkElem (Tag,"P",[]))
    bd <- (moveInc . elemIs) (MkElem (Tag,"P",[]))
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
        propertyImage = fmap (linkToURL site "") (getImage "src" im),
        propertyAgent = extractLiteral ag}

getRMPostcodes :: Chan (Maybe Property) -> String -> (URL,Cookies,FormVars)
    -> [Postcode] -> PriceRange -> BedsRange -> IO ()
getRMPostcodes _mergeChannel _ _ [] _ _ = return ()
getRMPostcodes mergeChannel site form1@(_action,_cookies,_vars) (p0:ps) pr bd = do
    (form2,beds,price) <- getRMForm2 site form1 p0
    hPutStr stderr $ (showString "RM " . shows (pickRange pr price) . shows (pickRange bd beds)) "\n"
    binarySubdivision site mergeChannel p0 pr bd form2 (pickRange bd beds) (pickRange pr price)
    getRMPostcodes mergeChannel site form1 ps pr bd

binarySubdivision :: String -> Chan (Maybe Property) -> Postcode -> PriceRange -> BedsRange -> (URL,Cookies,FormVars)
    -> BedsRanges -> PriceRanges -> IO ()
binarySubdivision site mergeChannel pc pr bd submit@(_,cookies,_) beds price = do
    hPutStr stderr $ (showString "RM: " . shows price . shows beds ) "\n"
    (rh,results) <- getRMInRange submit (mixRange beds) (mixRange price)
    -- hPutStr stderr $ showString (generateXML results) "\n"
    case gotAllProperties results of
        Just True -> do
            getRMPages site mergeChannel pc pr bd rh cookies results
        Just False -> do
            hPutStr stderr "rightmove: >100 ... splitting\n"
            case splitRange price of
                Just (p0,p1) -> do 
                    maybeClose rh
                    case p0 of
                        [] -> return ()
                        _ -> binarySubdivision site mergeChannel pc pr bd submit beds p0
                    case p1 of
                        [] -> return ()
                        _ -> binarySubdivision site mergeChannel pc pr bd submit beds p1
                _ -> do
                    hPutStr stderr "too many properties in range\n"
                    getRMPages site mergeChannel pc pr bd rh cookies results
        _ -> do
            maybeClose rh
            hPutStr stderr "rightmove site error - unable to find property count\n"
    
getRMInRange :: (URL,Cookies,FormVars) -> Maybe BedsRange -> Maybe PriceRange -> IO (Maybe Handle,DOM)
getRMInRange (action,cookies,vars) bedsRange priceRange = case priceRange of 
    Just (minPrice,maxPrice) -> case bedsRange of
        Just (minBeds,_maxBeds) -> do
            (h,src) <- getDocCook (MkElem (Document,action,setInputs [
                MkAttribute ("se_t","-1"),
                MkAttribute ("p_t","-1"),
                MkAttribute ("mi_b",bedsRMString minBeds),
                MkAttribute ("mi_p",priceRMString minPrice),
                MkAttribute ("ma_p",priceRMString maxPrice),
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

priceRMString :: Int -> String
priceRMString i 
    | i>3000000 = "-1"
    | i<=0 = "-1"
    | otherwise = showInt i ""

bedsRMString :: Int -> String
bedsRMString i
    | i>=5 = "5"
    | i<1 = "-1"
    | otherwise = showInt i ""

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
    _ <- (required . whileChar) isDigit
    _ <- (required . satisfy) (==';')
    return ()

stringToPrice :: String -> Maybe Int
stringToPrice s = case parseResult ( do
        (tryParser removeCharRef) `mplus` (return ())
        _ <- (tryParser $ untilParser (stringNcs "&pound;") char) `mplus` (return "")
        _ <- untilChar isDigit
        i <- parseInt
        return i) s of
    Ok i _ -> case i of 
            [] -> Nothing
            (i0:_)    | isDigit i0 -> Just (read i)
                      | otherwise -> Nothing
    _ -> Nothing

extractPrice :: DOM -> Maybe Int
extractPrice d = stringToPrice (extractString d)

stringToBeds :: String -> Maybe Int
stringToBeds s =  case parseResult ( do
        (tryParser removeCharRef) `mplus` (return ())
        _ <- untilChar isDigit
        i <- (required . whileChar) isDigit
        return i) s of
    Ok i _ -> case i of 
            [] -> Nothing
            (i0:_)    | isDigit i0 -> Just (read i)
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
extractLiteral d = stringToLiteral (tidyString $ extractString d) 

tidyString :: String -> String
tidyString (c0:cs)
    | (c0 == ',' || c0 == '.') = tidyString cs
    | isSpace c0 = tidyString cs
    | otherwise = c0:tidyString' cs
tidyString _ = " "

tidyString' :: String -> String
tidyString' (c0:cs)
    | (c0 == ',' || c0 == '.') = c0:tidyString'' cs
    | isSpace c0 = tidyString'' cs
    | otherwise = c0:tidyString' cs
tidyString' _ = ""

tidyString'' :: String -> String
tidyString'' (c0:cs)
    | (c0 == ',' || c0 == '.') = c0:tidyString'' cs
    | isSpace c0 = tidyString'' cs
    | otherwise = ' ':c0:tidyString' cs
tidyString'' _ = ""

--------------------------------------------------------------------

getPPForm :: String -> IO (Maybe ((URL,Cookies,FormVars),PriceRanges,BedsRanges))
getPPForm site = do
    (u2,h,src) <- getPageWithRedirects [] [] ("http://"++site++"/_flash/buy.php")
    (case filterDOM (do
            alter tidyDOM
            c <- getCookies
            (m,a) <- matchForm "search"
            rnf (m,a,c) `seq` return (m,if a=="#" then u2 else a,c)) src of
        DOMOut (m,a,c) f -> case filterDOM (do
                    processForm
                    v <- getInputs  
                    rnf v `seq` return v) f of
                DOMOut v _ -> case filterDOM (do
                            minPrices <- matchSelect getOptions "Minprice"
                            maxPrices <- matchSelect getOptions "Maxprice"
                            minBeds <- matchSelect getOptions "Minbedrooms"
                            rnf (minPrices,maxPrices,minBeds) `seq` return 
                                (makeRanges (map read minPrices) (map read maxPrices),
                                zip (map read minBeds) zeroList)) f of
                        DOMOut (p,b) _ -> do
                            (m,a,c,v,p,b) `seq` maybeClose h
                            return $ Just ((linkToURL site m a,c,v),p,b)
                        _ -> do
                            maybeClose h
                            return Nothing
                _ -> do
                    maybeClose h
                    return Nothing
        _ -> do
            maybeClose h
            return Nothing) `Exception.catch` (\(e::SomeException) -> do maybeClose h; throw e)

getPPRanges :: String -> PriceRange -> BedsRange
    -> IO (Maybe ((URL,Cookies,FormVars),PriceRanges,BedsRanges))
getPPRanges site pr bd = do
    r <- getPPForm site
    case r of
        Just (form,p,b) -> return $ Just (form,pickRange pr p,pickRange bd b) 
        _ -> return Nothing

getLinks :: DOM -> [String]
getLinks [] = []
getLinks ((_i,e):d) = case e of
    (MkElem (Tag,"A",as)) -> getAttributeAsString "href" as:getLinks d
    _ -> getLinks d

filterLinks :: FilterDOM [String]
filterLinks = FilterDOM (\d -> case d of
    ((_i,e):d') -> case e of
        (MkElem (Tag,"A",as)) -> case filterDOM filterLinks d' of
            DOMOut bs d'' -> DOMOut (getAttributeAsString "href" as:bs) d''
            _ -> DOMOut [] d'
        _ -> filterDOM filterLinks d'
    _-> DOMOut [] d)

getPPPostcodes :: String -> Chan (Maybe Property) -> (URL,Cookies,FormVars)
    -> [Postcode] -> PriceRanges -> BedsRanges -> PriceRange -> BedsRange -> IO ()
getPPPostcodes _ _mergeChannel _ [] _ _ _ _ = return ()
getPPPostcodes site mergeChannel form1 (p0:ps) pr bd p b = do
    (rh,results) <- getPPInRange form1 p0 (mixRange bd) (mixRange pr)
    (pages,ranges) <- ( do
        ls <- return $ getPPLinkSection results
        pages <- return ((getPPPLinks (site++"/php") "get" ls) `using` rdeepseq)
        ranges <- return ((getPPRLinks (site++"/php") "get" ls) `using` rdeepseq)
        getPPProperties mergeChannel site p0 p b results
        return (pages,ranges)) `finally` (maybeClose rh)
    getPPSubPage pages form1 mergeChannel site p0 p b
    getPPRangePage site ranges form1 mergeChannel p0 p b
    getPPPostcodes site mergeChannel form1 ps pr bd p b

getPPRangePage :: String -> [URL] -> (URL,Cookies,FormVars) -> Chan (Maybe Property)
    -> Postcode -> PriceRange -> BedsRange -> IO ()
getPPRangePage _ [] _ _ _ _ _ = return ()
getPPRangePage site (u0:us) f1@(_,cook,vars) mergeChannel p0 p b = do
    (rh,ranr) <- getDocCook (MkElem (Document,u0,vars)) cook
    ls <- return $ getPPLinkSection ranr
    pages <- return $ getPPPLinks (site++"/php") "get" ls
    getPPProperties mergeChannel site p0 p b (fTidyDOM ranr)
    maybeClose rh
    getPPSubPage pages f1 mergeChannel site p0 p b 
    getPPRangePage site us f1 mergeChannel p0 p b

getPPSubPage :: [URL] -> (URL,Cookies,FormVars) -> Chan (Maybe Property) -> String -> Postcode -> PriceRange -> BedsRange -> IO ()
getPPSubPage [] _ _ _ _ _ _ = return ()
getPPSubPage (_:[]) _ _ _ _ _ _ = return () -- discard print_all
getPPSubPage (u0:us@(_:_)) f1@(_,cook,vars) mergeChannel site p0 p b = do
    (sh,subr) <- getDocCook (MkElem (Document,u0,vars)) cook
    getPPProperties mergeChannel site p0 p b (fTidyDOM subr)
    maybeClose sh
    getPPSubPage us f1 mergeChannel site p0 p b

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

getPPProperties :: Chan (Maybe Property) -> String -> Postcode -> PriceRange -> BedsRange -> DOM -> IO ()
getPPProperties mergeChannel site pc pr bd d = do
    -- hPutStr stderr $ showString (generateXML d) "\n"
    case filterDOM (do
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            discardBranches 1) d of
        DOMOut _ d' -> do
            -- hPutStr stderr $ showString (generateXML d') "\n"
            getPPLoop mergeChannel site pc pr bd d'
        _ -> return ()

getPPLoop :: Chan (Maybe Property) -> String -> Postcode -> PriceRange -> BedsRange -> DOM -> IO ()
getPPLoop mergeChannel site pc pr bd d = case filterDOM (ppPropertyFilter site pc) d of
    DOMOut (Just p) d' -> do
        -- hPutStr stderr $ shows p "\n"
        checkRange mergeChannel pr bd p 
        getPPLoop mergeChannel site pc pr bd d'
    DOMOut Nothing d' -> getPPLoop mergeChannel site pc pr bd d'
    _ -> return ()

ppPropertyFilter :: String -> Postcode -> FilterDOM (Maybe Property)
ppPropertyFilter site pc = do
    (from . elemIs) (MkElem (Tag,"TABLE",[]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"B",[]))
    addr <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
    -- (from . elemIs) (MkElem (Tag,"A",[]))
    im <- (moveInc . elemIs) (MkElem (Tag,"I",[]))
    pdat <- (moveExc . elemIs) (MkElem (Tag,"A",[]))
    dtls <- (moveInc . elemIs) (MkElem (Tag,"A",[]))
    agent <- (moveInc . elemIs) (MkElem (Tag,"A",[]))
    return $! case parseResult parsePPProperty (extractString pdat) of
        Ok pp _ -> Just (pp {
            propertySite = Just "PP",
            propertyPostcode = Just pc,
            propertyImage = fmap (linkToURL site "") (getImage "src" im),
            propertyAgent = extractLiteral agent,
            propertyDetails = fmap (linkToURL site "") (getLink "href" dtls),
            propertyAddress = extractLiteral addr})
        _ -> Nothing

parsePPBeds' :: Parser String
parsePPBeds' = do
    a <- (required . whileChar) isDigit
    _ <- whileChar isSpace
    stringNcs "bedroom"
    return a

parsePPBeds :: Parser String
parsePPBeds = matchParser parsePPBeds' char 
    
stringToPPBeds :: String -> Maybe Int
stringToPPBeds s = case parseResult parsePPBeds s of
    Ok i _ -> case i of
        [] -> Nothing
        (i0:_) | isDigit i0 -> Just (read i)
                    | otherwise -> Nothing
    _ -> Nothing

parsePPProperty :: Parser Property
parsePPProperty = do
    _ <- untilParser (stringNcs "&#0149;") char
    beds <- untilParser (stringNcs "&#0149;") char
    desc <- untilParser (stringNcs "&pound;") char
    price <- untilParser (stringNcs "&#0149;") char
    return $ makeProperty {
        propertyBeds = stringToPPBeds beds,
        propertyDescription = stringToLiteral desc,
        propertyPrice = stringToPrice price}

getPPInRange :: (URL,Cookies,FormVars) -> Postcode -> Maybe BedsRange -> Maybe PriceRange
    -> IO (Maybe Handle,DOM)
getPPInRange (action,cookies,vars) pc bedsRange priceRange = case priceRange of 
    Just (minPrice,maxPrice) -> case bedsRange of
        Just (minBeds,_maxBeds) -> do
            (h,src) <- getDocCook (MkElem (Document,action,setInputs [
                --MkAttribute ("page","1"),
                --MkAttribute ("subpage","1"),
                MkAttribute ("per_page","100"),
                MkAttribute ("postcodes",","++pc++","),
                MkAttribute ("Minprice",pricePPString minPrice),
                MkAttribute ("Maxprice",pricePPString maxPrice),
                MkAttribute ("Minbedrooms",bedsPPString minBeds),
                MkAttribute ("searchType","buy")
                ] vars)) cookies
            return (h,fTidyDOM src)
        Nothing -> return (Nothing,[])
    Nothing -> return (Nothing,[])

pricePPString :: Int -> String
pricePPString i 
    | i>=10000000 = "100000000"
    | i<=0 = "0"
    | otherwise = showInt i ""

bedsPPString :: Int -> String
bedsPPString i
    | i>=8 = "8"
    | i<=0 = "0"
    | otherwise = showInt i ""

searchPureproperty :: URL -> Chan (Maybe Property) -> [Postcode] -> PriceRange -> BedsRange -> IO ()
searchPureproperty site mergeChannel pc p b = do
    r <- getPPRanges site p b
    case r of
        Just (_form1@(_ac,cook,_vars),pr,bd) -> do
            hPutStr stderr $ (showString "PP " . shows pr . shows bd) "\n"
            getPPPostcodes site mergeChannel ("http-get://"++site++"/php/search.php",cook,[]) pc pr bd p b
        _ -> hPutStr stderr $ (showString "error parsing form: " . showString site) "\n"

------------------------------------------------------------------------------
-- propertyfinder

getInts :: [String] -> [Int]
getInts [] = []
getInts (t0:ts) = case parseResult (do
        _ <- untilChar isDigit
        i <- parseInt
        return (read i)) t0 of
    Ok i _ -> i:getInts ts
    _ -> 0:getInts ts

pricePFString :: Int -> String
pricePFString i 
    | i>=10000000 = "10000000+"
    | i<=10000 = "10000"
    | otherwise = showInt i ""

bedsPFString :: Int -> String
bedsPFString i
    | i>=6 = "6+"
    | i<=0 = "0"
    | otherwise = showInt i ""

getPFForm :: String -> IO (Maybe ((URL,Cookies,FormVars),PriceRanges,BedsRanges))
getPFForm site = do
    (u2,h,src) <- getPageWithRedirects [] [] ("http://"++site++"/")
    (case filterDOM (do
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
                    rnf (minPrices,maxPrices, minBeds,maxBeds) `seq` return
                        (makeRanges (getInts minPrices) (getInts maxPrices),
                        makeRanges (getInts minBeds) (getInts maxBeds))) f of
                DOMOut (p,b) _ -> do
                    rnf (m,a,c,v,p,b) `seq` maybeClose h
                    return $ Just ((linkToURL site m a,c,v),p,b)
                _ -> do
                    maybeClose h
                    return Nothing
            _ -> do
                maybeClose h
                return Nothing
        _ -> do
            maybeClose h
            return Nothing) `Exception.catch` (\(e::SomeException) -> do maybeClose h; throw e)

searchPropertyfinder :: URL -> Chan (Maybe Property) -> [Postcode] -> PriceRange -> BedsRange -> IO ()
searchPropertyfinder site mergeChannel pc pr bd = do
    form <- getPFForm site
    case form of
        Just f -> getPFPostcodes mergeChannel site f pc pr bd
        Nothing -> hPutStr stderr "could not get propertyfinder form\n"

getPFPostcodes :: Chan (Maybe Property) -> String -> ((URL,Cookies,FormVars),BedsRanges,PriceRanges)
    -> [Postcode] -> PriceRange -> BedsRange -> IO ()
getPFPostcodes _ _ _ [] _ _ = return ()
getPFPostcodes mergeChannel site form@(submit,price,beds) (p0:ps) pr bd = do
    hPutStr stderr $ (showString "PF " . shows (pickRange pr price) . shows (pickRange bd beds)) "\n"
    subdividePF mergeChannel site p0 pr bd submit (pickRange bd beds) (pickRange pr price)
    getPFPostcodes mergeChannel site form ps pr bd
    
subdividePF :: Chan (Maybe Property) -> String -> Postcode -> PriceRange -> BedsRange
    -> (URL,Cookies,FormVars) -> BedsRanges -> PriceRanges -> IO ()
subdividePF mergeChannel site pc pr bd submit@(_,cookies,_) beds price = do
    hPutStr stderr $ (showString "PF: " . shows price . showChar ' ' . shows beds) "\n"
    (rh,results) <- getPFInRange submit pc (mixRange beds) (mixRange price)
    case gotAllPFProperties results of
        Just True -> getPFPages mergeChannel site pc pr bd rh cookies results
        Just False -> case splitRange price of
            Just (p0,p1) -> do
                maybeClose rh
                case p0 of
                    [] -> return ()
                    _ -> subdividePF mergeChannel site pc pr bd submit beds p0
                case p1 of
                    [] -> return ()
                    _ -> subdividePF mergeChannel site pc pr bd submit beds p1
            _ -> case splitRange beds of
                Just (b0,b1) -> do
                    maybeClose rh
                    case b0 of
                        [] -> return ()
                        _ -> subdividePF mergeChannel site pc pr bd submit b0 price
                    case b1 of 
                        [] -> return ()
                        _ -> subdividePF mergeChannel site pc pr bd submit b1 price
                _ -> do
                    hPutStr stderr "propertyfinder: too many properties in range\n"
                    getPFPages mergeChannel site pc pr bd rh cookies results
        _ -> do
            maybeClose rh
            hPutStr stderr "propertyfinder: no properties in range\n"

getPFPages :: Chan (Maybe Property) -> String -> Postcode -> PriceRange -> BedsRange
    -> Maybe Handle -> Cookies -> DOM -> IO ()
getPFPages mergeChannel site pc pr bd h cookies d = do
    ls <- return $ getPFLinks site "get" d
    hPutStr stderr $ (showString "PF: " . shows ls) "\n"
    getPFProperties mergeChannel site pc pr bd d
    rnf ls `seq` maybeClose h
    getPFNext mergeChannel site ls pc pr bd cookies

getPFNext :: Chan (Maybe Property) -> String -> [URL] -> Postcode -> PriceRange -> BedsRange
    -> Cookies -> IO ()
getPFNext _ _ [] _ _ _ _ = return ()
getPFNext mergeChannel site (u0:us) pc pr bd cookies = do
    -- hPutStr stderr $ (showString "PF: " . showString u0) "\n"
    (sh,sub) <- getDocCook (MkElem (Document,u0,[])) cookies
    getPFProperties mergeChannel site pc pr bd sub
    maybeClose sh
    getPFNext mergeChannel site us pc pr bd cookies

getPFProperties :: Chan (Maybe Property) -> String -> Postcode -> PriceRange -> BedsRange
    -> DOM -> IO ()
getPFProperties _ _ _ _ _ [] = return ()
getPFProperties mergeChannel site pc pr bd d = do
    -- putStrLn (generateXML d)
    case filterDOM (do
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            select [3]
            cut 1
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            select [0]
            cut 1
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            select [0]
            cut 1
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            discardBranches 3) d of
            -- select [2]
            -- cut 1
            -- (graft . elemIs) (MkElem (Tag,"TABLE",[]))
            -- discardBranches 1) d of
        DOMOut _ d' -> do
            getPFLoop mergeChannel site pc pr bd d'
        _ -> return ()

pfPropertyFilter :: String -> Postcode -> FilterDOM Property
pfPropertyFilter site pc = do
    (from . elemIs) (MkElem (Tag,"TABLE",[MkAttribute ("class","blue")]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    pr <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
    bd <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"TABLE",[]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    dt <- (moveExc . elemIs) (MkElem (Tag,"IMG",[]))
    im <- (moveInc . elemIs) (MkElem (Tag,"TD",[])) 
    dc <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    (from . elemIs) (MkElem (Tag,"TD",[]))
    ad <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
    ag <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
    al <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
    return $! makeProperty {
        propertySite = Just "PF",
        propertyPostcode = Just pc,
        propertyPrice = extractPrice pr,
        propertyBeds = stringToPPBeds (extractString bd),
        propertyDescription = extractLiteral dc,
        -- propertyDescription = Just (shows dt "\n"),
        propertyAddress = extractLiteral ad,
        propertyAgent = extractLiteral ag,
        propertyAgentLink = fmap (linkToURL site "") (getLink "href" al),
        propertyImage = fmap (linkToURL site "") (getImage "src" im),
        propertyDetails = fmap (linkToURL site "") (getLink "href" dt)}

getPFLoop :: Chan (Maybe Property) -> String -> Postcode -> PriceRange -> BedsRange -> DOM -> IO ()
getPFLoop _ _ _ _ _ [] = return ()
getPFLoop mergeChannel site pc pr bd d = do
    {-putStrLn (generateXML d)-}
    case filterDOM (pfPropertyFilter site pc) d of
        DOMOut p d' -> do
            -- hPutStr stderr $ (showString "PF: " . shows p) "\n"
            checkRange mergeChannel pr bd p
            getPFLoop mergeChannel site pc pr bd d'
        _ -> do
            hPutStr stderr $ "PF: empty results page.\n"
            return ()

getPFLinks :: String -> String -> DOM -> [String]
getPFLinks site m d = case filterDOM (do
        (graft . elemIs) (MkElem (Tag,"TABLE",[]))
        select [2]
        cut 1
        (graft . elemIs) (MkElem (Tag,"A",[]))
        l <- filterLinks
        rnf l `seq` return l) d of
    DOMOut l _ -> map (linkToURL site m) l
    _ -> []

getPFInRange :: (URL,Cookies,FormVars) -> Postcode -> Maybe BedsRange -> Maybe PriceRange
    -> IO (Maybe Handle,DOM)
getPFInRange (action,cookies,vars) pc bedsRange priceRange = case priceRange of
    Just (minPrice,maxPrice) -> case bedsRange of
        Just (minBeds,maxBeds) -> do
            (h,src) <- getDocCook (MkElem (Document,action,setInputs [
                MkAttribute ("Minprice",pricePFString minPrice),
                MkAttribute ("Maxprice",pricePFString maxPrice),
                MkAttribute ("Minbedrooms",bedsPFString minBeds),
                MkAttribute ("Maxbedrooms",bedsPFString maxBeds),
                MkAttribute ("searchType","both"),
                MkAttribute ("postcode",pc),
                MkAttribute ("sorting","down")
                ] vars)) cookies
            -- hPutStr stderr $ showString (generateXML src) "\n"
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
    DOMOut _ d' -> Just (case parseResult (matchParser (stringNcs "more than") char) (extractString d') of
        Ok _ _ -> False
        _ -> True)
    _ -> Nothing

