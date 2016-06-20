{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}


-- main.hs (C)2001 Keean Schupke
--
--      main program, uses DOM module 

module Main(main) where
import GHC.Base
import Data.Char
--import Data.Int
import System.IO
--import System.Random
import Lib.MetaSearch.Parser as Parser
import Lib.MetaSearch.DOM
import Lib.MetaSearch.Filter
import Lib.MetaSearch.Forest
import Lib.MetaSearch.Forms
import Lib.MetaSearch.Cookies 
--import qualified Data.Map as Map
--import Lib.MetaSearch.Shuffle
import Lib.MetaSearch.Redirect
import Numeric
--import Control.Parallel.Strategies
import Control.DeepSeq

type Postcode = String
type PCodeList = String
--type Key = String
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
type Street = String
type Area = String
type City = String
type FormVars = [Attribute]
--type Method = String

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
postcodes = ["N1"]

pcodeList :: PCodeList
pcodeList = join' postcodes where

    join' :: [Postcode] -> PCodeList
    join' (p0:ps) = foldl (\a b -> a ++ (',':b)) p0 ps
    join' [] = ""

getKey :: IO (URL,Cookies,FormVars)
getKey = do
    (_,frm) <- getDocument (MkElem (Document,"http-get://www.primelocation.com/",[]))
    -- frm <- getDocument (MkElem (Document,"file:primelocation.com",[]))
    case filterDOM (do
        alter tidyDOM
        c <- getCookies
        (_,a) <- matchForm "searchForm"
        processForm
        f <- getInputs
        return (a,c,f)) frm of
        DOMOut ei _ -> return $! ei -- strict: forces close of file before return
        DOMVoid -> undefined

getAgents :: IO [Agent]
getAgents = (do
    print pcodeList
    (_,src) <- getDocument (MkElem (Document,"http-get://www.primelocation.com/agentSearch/agentSearch.cfm",[
        MkAttribute ("whichsearch","all"), MkAttribute ("searchClass","agent"), 
        MkAttribute ("location",pcodeList)]))
    -- hPutStr stderr (generateXML src)
    case filterDOM (do alter tidyDOM; _ <- matchForm "ladder"; matchSelect getOptionsText "lst") src of
        DOMOut lst _ -> return $! map filterAgent lst
        _ -> return []) where

    filterAgent :: String -> Agent
    filterAgent as = case parseResult ( do
        _ <- untilChar (==':')
        _ <- (skipSpace . whileChar) (==':')
        b <- untilParserOrEnd (string " - ") char
        return b) as of
            Ok r _ -> reverse r
            _ -> reverse as

insertAgents :: [String] -> String -> [String] -> IO [String]
insertAgents [] _ c = do
    putStr "\n"
    return c
insertAgents (a:as) b c = if a /= b
    then do
        putStr ((showString (reverse a) . showString "\n") "")
        x <- insertAgents as a (a:c)
        return x
    else do
        x <- insertAgents as b c
        return x
    
getRanges :: IO ([PriceRange],[BedsRange])
getRanges = do
    (_,frm) <- getDocument (MkElem (Document,"http-get://www.primelocation.com/",[]))
    -- frm <- getDocument (MkElem (Document,"file:primelocation.com",[]))
    -- hPutStr stderr (generateXML frm)
    case filterDOM (do alter tidyDOM; _ <- matchForm "searchForm";(graft . elemIs) (MkElem (Tag,"SELECT",[]))) frm of
        DOMOut _ sel -> do
            return $! ((zip (map read $ domToSelect getOptions "minprice" sel)
                (map read $ domToSelect getOptions "maxprice" sel)),
                (zip (map read $ domToSelect getOptions "minbedrooms" sel)
                (map read $ domToSelect getOptions "maxbedrooms" sel)))
        _ -> do
            -- hPutStr stderr "[EMPTY]\n"
            return ([],[])
            

getInRange :: URL -> [Attribute] -> Maybe PriceRange -> Maybe BedsRange -> Postcode -> Cookies -> IO DOM
getInRange url key priceRange bedsRange postcode cookies = case priceRange of 
    Just (minPrice,maxPrice) -> case bedsRange of
        Just (minBeds,maxBeds) -> do
            -- hPutStr stderr $ (showChar '<' . showInt minBeds . showChar '-' . showInt maxBeds . showChar ',' .
            --  showInt minPrice . showChar '-' . showInt maxPrice) ">"
            -- src <- getDocument (MkElem (Document,"file:test.html",[]))
            hsrc <- getDocCook (MkElem (Document,"http-get://www.primelocation.com/"++url,
                setInputs [MkAttribute ("place",postcode), MkAttribute ("maxprice",show maxPrice),
                MkAttribute ("minprice",show minPrice), MkAttribute ("minbedrooms",show minBeds),
                MkAttribute ("maxbedrooms",show maxBeds), MkAttribute ("propertyType","ALL"),
                MkAttribute ("criteria","0"), MkAttribute ("pricePeriod","w"), MkAttribute ("keyword0p",""),
                MkAttribute ("rental","0"), MkAttribute ("searchClass","res"), MkAttribute ("Update","0"),
                MkAttribute ("diffnav","1"), MkAttribute ("popup","0")] key)) cookies
            -- hPutStr stderr (generateXML src)
            (_h,rdr) <- followRedirects "http-get://www.primelocation.com" hsrc cookies
            return $! rdr
        Nothing -> return []
    Nothing -> return []

parsePrice :: Parser String
parsePrice = do
    p <- (required . whileChar) isDigit
    _ <- whileChar (==',')
    ps <- parsePrice `mplus` return ""
    return (p++ps)

parseAgents :: [Agent] -> Parser Agent
parseAgents ps = Parser (\cs -> parse (tryAgents ps) (reverse cs)) where

    tryAgents :: [Agent] -> Parser Agent
    tryAgents [] = Parser (\_ -> Empty Error)
    tryAgents (p:ps') = Parser (\cs -> case parse (matchParser (stringNcs p) char) cs of
        Consumed (Ok () r) -> Consumed $ Ok (reverse p) (reverse r)
        _ -> parse (tryAgents ps') cs)

parseAddress :: Parser (Street,Area,City)
parseAddress = do
    street <- (required . untilChar) (==',')
    _ <- (skipSpace . whileChar) (==',')
    area <- untilChar (==',')
    _ <- (skipSpace . whileChar) (==',')
    city <- untilEnd
    return (street,area,city)

parseAddrAgent :: [Agent] -> Parser (Street,Area,City,Agent)
parseAddrAgent agents = do
    agent <- parseAgents agents
    (s,a,c) <- parseAddress
    return (s,a,c,agent)

parseProperty :: [Agent] -> Parser (Price,Beds,(Street,Area,City,Agent))
parseProperty agents = do
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
    return $! (getPrice price,read bedrooms,getAddress agents addr agent) where
        
        getPrice :: String -> Price
        getPrice p = case parseResult parsePrice p of
            Ok p' _ -> read p'
            _ -> -1

        getAddress :: [Agent] -> String -> Agent -> (Street,Area,City,Agent)
        getAddress agents' addr agent = if agent /= ""
            then case parseResult parseAddress addr of
                Ok (s,a,c) _ -> case parseResult (parseAgents agents') agent of
                    Ok g _ -> (s,a,c,g)
                    _ -> (s,a,c,agent)
                _ -> (addr,"","London",agent)
            else case parseResult (parseAddrAgent agents') addr of
                Ok a _ -> a
                _ -> (addr,"","London",agent)

doInserts :: Postcode -> Cookies -> [(String,String)] -> [Agent] -> [MetaResult]
doInserts _ _ [] _ = []
doInserts postcode cookies ((text,_link):rs) agents = ( do
    case parseResult (parseProperty agents) text of
        Ok (price,bedrooms,(addr0,addr1,addr2,agent)) _ -> 
            (postcode,price,bedrooms,concat [addr0,addr1,addr2],agent):doInserts postcode cookies rs agents
        _ -> doInserts postcode cookies rs agents) where

    dbString :: String -> ShowS
    dbString [] = (\a -> a)
    dbString (s0:ss) = (\a -> case s0 of 
        '\'' -> '\\':s0:dbString ss a
        ',' -> ' ':dbString ss a
        _ -> s0:dbString ss a)

joinLists :: [a] -> [a] -> [a]
joinLists [] b = b
joinLists (a0:as) b = a0:joinLists as b

-- recursive split if results full
getPostcode :: [PriceRange] -> [BedsRange] -> Postcode -> [Agent] -> IO [MetaResult]
getPostcode prices beds pcode agents = do
    (url,cookies,key) <- getKey
    print url
    print cookies
    print key
    src <- getInRange url key (mixRange prices) (mixRange beds) pcode cookies
    -- case filterDOM (do alter tidyDOM; matchForm "ladder"; cut 1) src of
    -- hPutStr stderr $ generateXML src
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
            str <- return $ extractString frm
            case parseResult parseComplete str of
                Ok _ _ -> do
                    hPutStr stderr "."
                    extractProperties pcode cookies frm agents
                _ -> case parseResult parseIncomplete str of 
                    Ok _ _ -> case splitRange prices of
                        Just (p0,p1) -> do
                            hPutStr stderr "("
                            a <- getPostcode p0 beds pcode agents
                            b <- getPostcode p1 beds pcode agents
                            hPutStr stderr ")"
                            return $ joinLists a b
                        _ -> case splitRange beds of
                            Just (b0,b1) -> do
                                hPutStr stderr "["
                                a <- getPostcode prices b0 pcode agents
                                b <- getPostcode prices b1 pcode agents
                                hPutStr stderr "]"
                                return $ joinLists a b
                            _ -> do
                                hPutStr stderr "!"
                                extractProperties pcode cookies frm agents
                    _ -> do
                        hPutStr stderr "X"
                        return []
        _ -> do
            hPutStr stderr "?"
            return []
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

    extractProperties :: Postcode -> Cookies -> DOM -> [Agent] -> IO [MetaResult]
    extractProperties pcode' _cookies frm _agents = case filterDOM ( do
            (graft . elemIs) (MkElem (Tag,"TABLE",[]))) frm of
            -- text <- matchSelect getOptionsText "propertyid"
            -- link <- matchSelect getOptions "propertyid"
            -- return (zip text link)) frm of
            -- DOMOut lst _ -> return $ doInserts pcode' cookies lst agents
        DOMOut _ d' -> do
            getPLLoop pcode' d'
            return []
        _ -> return []

getPLLoop :: Postcode -> DOM -> IO ()
getPLLoop pc d = case filterDOM (do
        (from . elemIs) (MkElem (Tag,"TABLE",[]))
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
        return $ (im,makeProperty {
            propertyAddress = extractLiteral ad,
            propertyPrice = extractPrice pr,
            propertyBeds = extractBeds bd,
            propertyDescription = extractLiteral dc,
            propertyImage = Just (getImage im),
            propertyAgent = Just (getImage ag)})) d of
    DOMOut (a,p) d' -> do
        putStr $ showString (generateXML a) "\n"
        print p
        getPLLoop pc d'
    _ -> return ()

getProperties :: [PriceRange] -> [BedsRange] -> [Postcode] -> [Agent] -> IO [MetaResult]
getProperties [] _ _ _ = return []
getProperties _ [] _ _ = return []
getProperties _ _ [] _ = return []
getProperties prices beds _postcodes@(p0:ps) agents = do
    hPutStr stderr ("{"++p0)
    p0' <- getPostcode prices beds p0 agents
    hPutChar stderr '}'
    ps' <- getProperties prices beds ps agents
    return (p0'++ps')

splitRange :: [a] -> Maybe ([a],[a])
splitRange l = splitRange' 1 (round ((toRational (length l)) / 2.0)) l [] where

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

type MetaResult = (String,Int,Int,String,String)

-- metaSearch :: String -> Int -> Int -> Int -> Int -> IO MetaResult
-- metaSearch as pr0 pr1 bd0 bd1 = do
    -- pcodes <- parsePcodes as
    -- result <- searchPrimelocation pcodes pr0 pr1 bd0 bd1
    -- return result

pickRange :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
pickRange _ [] = []
pickRange (pmin,pmax) [(i,j)]
    | i<=pmin && j>=pmax = [(i,j)]
    | otherwise = []
pickRange (pmin,pmax) (_r0@(p0,q0):rs@((p1,_):_)) = if (if p1 < 0 then 0 else p1) <= pmin
    then pickRange (pmin,pmax) rs
    else (if p0 < 0 then 0 else p0,if q0 < 0 then maxInt else q0):pickMax pmax rs

pickMax :: Int -> [(Int,Int)] -> [(Int,Int)]
pickMax _ [] = []
pickMax pmax (_r0@(p0,q0):rs) = let q' = if q0 <= 0 then maxInt else q0
    in if pmax >= q'
        then (if p0 < 0 then 0 else p0,q'):pickMax pmax rs
        else []

searchPrimelocation :: [Postcode] -> (Int,Int) -> (Int,Int) -> IO [MetaResult]
searchPrimelocation pcds pr bd = do
    (prices',bedrooms) <- getRanges
    prices <- return $ pickRange pr prices'
    beds <- return $ pickRange bd bedrooms
    -- pcodes <- shuffle pcds
    -- agents <- getAgents
    -- print (pcodes,prices,beds,agents)
    getProperties prices beds pcds []

showMeta :: [MetaResult] -> IO ()
showMeta [] = return ()
showMeta (m0:ms) = do
    print m0
    showMeta ms

main :: IO ()
main = do
    mr <- searchPrimelocation ["N1"] (100000,200000) (2,3)
    showMeta mr
--  hPutStr stderr "Fetching search ranges...\n"
--  (prices,bedrooms) <- getRanges
--  print prices
--  print bedrooms
--  hPutStr stderr "Shuffling postcodes...\n"
--  randPcodes <- shuffle postcodes
--  hPutStr stderr "Fetching agent list:"
--  agents_tmp <- getAgents
--  hPutStr stderr (show $ length agents_tmp)
--  hPutChar stderr '\n'
--  agents <- insertAgents agents_tmp "" []
--  hPutStr stderr "Fetching properties by postcode...\n"
-- getProperties prices bedrooms randPcodes agents
--  searchPureproperty "www.pureproperty.com" ["SW19"] (0,300000) (2,3)
-- searchRightmove "www.rightmove.co.uk"
--  searchPropertyfinder "www.propertyfinder.com" ["SW19"] (100000,300000) (2,3)

getUrlVars :: String -> [Attribute]
getUrlVars "" = []
getUrlVars s = case parseResult (Parser.many $ do
        a <- (required . untilChar) (\c -> c=='=' || c==';' || c=='&')
        _ <- whileChar (=='=')
        b <- untilChar (\c -> c==';' || c=='&')
        _ <- whileChar (\c -> c==';' || c=='&')
        return (MkAttribute (a,b))) s of
    Ok v _ -> v
    _ -> []


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
    propertyImage :: Maybe String,
    propertyAgent :: Maybe String} deriving Show

makeProperty :: Property
makeProperty = Property {
    propertyPostcode = Nothing,
    propertyPrice = Nothing,
    propertyBeds = Nothing,
    propertyType = Nothing,
    propertyAddress = Nothing,
    propertyDescription = Nothing,
    propertyImage = Nothing,
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
    (from . elemIs) (MkElem (Tag,"TD",[]))
    ag <- (moveExc . elemIs) (MkElem (Tag,"TABLE",[]))
    nxt <- (parseMeta pc) `mplus` (return [])
    -- return ([extractString pr,extractString bd,extractString ty,extractString ad,extractString de,
    --  extractString x]:nxt)
    return $ makeProperty {
        propertyPostcode = Just pc,
        propertyPrice = extractPrice pr,
        propertyBeds = extractBeds bd,
        propertyType = extractLiteral ty,
        propertyAddress = extractLiteral ad,
        propertyDescription = extractLiteral de,
        propertyAgent = extractLiteral ag}:nxt

removeCharRef :: Parser ()
removeCharRef = (tryParser (do
    stringNcs "&#"
    _ <- (required . whileChar) isDigit
    _ <- (required . satisfy) (==';')
    return ())) `mplus` (return ())

extractPrice :: DOM -> Maybe Int
extractPrice d = case parseResult ( do
        removeCharRef
        _ <- untilParser (stringNcs "&pound;") char
        _ <- untilChar isDigit
        i <- parsePrice
        return (read i)) (extractString d) of
    Ok i _ -> Just i
    _ -> Nothing
        
extractBeds :: DOM -> Maybe Int
extractBeds d = case parseResult ( do
        removeCharRef
        _ <- untilChar isDigit
        i <- (required . whileChar) isDigit
        return (read i)) (extractString d) of
    Ok i _ -> Just i
    _ -> Nothing

extractLiteral :: DOM -> Maybe String
extractLiteral [] = Nothing
extractLiteral d = case parseResult removeCharRef (extractString d) of
    Ok _ i -> Just i
    _ -> Nothing

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
        Just (minBeds,_maxBeds) -> do
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

--------------------------------------------------------------------
-- search pureproperty...

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
    fm [] _ = Nothing
    fm (i0:[]) [] = Just i0
    fm (_i0:is@(_:_)) [] = fm is y
    fm i@(i0:_is) (j0:js)
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
getLinks ((_i,e):d) = case e of
    (MkElem (Tag,"A",as)) -> getAttributeAsString "href" as:getLinks d
    _ -> getLinks d

getImage :: DOM -> String
getImage [] = []
getImage ((_i,e):d) = case e of
    (MkElem (Tag,"IMG",as)) -> getAttributeAsString "src" as
    _ -> getImage d

filterLinks :: FilterDOM [String]
filterLinks = FilterDOM (\d -> case d of
    ((_i,e):d') -> case e of
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
    hPutStr stderr $ showString (generateXML results) "\n"
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
    addr <- (moveInc . elemIs) (MkElem (Tag,"A",[]))
    (from . elemIs) (MkElem (Tag,"I",[]))
    beds <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
    (from . elemIs) (MkElem (Tag,"DIV",[]))
    desc <- (moveInc . elemIs) (MkElem (Tag,"BR",[]))
    price <- (moveInc . elemIs) (MkElem (Tag,"A",[]))
    agent <- (moveInc . elemIs) (MkElem (Tag,"A",[]))
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
        Just (minBeds,_maxBeds) -> do
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
        Just (h,form1@(_ac,cook,_vars),pr',bd') -> do
            print form1
            print pr' 
            print bd'
            getPPPostcodes site h ("http-get://"++site++"/php/search.php",cook,[]) pc pr' bd'
        _ -> hPutStr stderr $ (showString "error parsing form: " . showString site) "\n"

------------------------------------------------------------------------------

getInts :: [String] -> [Int]
getInts [] = []
getInts (t0:ts) = case parseResult (do
        _ <- untilChar isDigit
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
    pr' <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
    bd' <- (moveInc . elemIs) (MkElem (Tag,"TD",[]))
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
    return (extractPrice pr',extractString bd',extractString dc,extractString ad,extractString ag)) d of
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

