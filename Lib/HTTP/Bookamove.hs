{-# LANGUAGE ScopedTypeVariables #-}

-- server.hs: Copyright (C)2002 Keean Schupke
--
--      HyperServer main module

module Lib.HTTP.Bookamove (getHandler) where

--import GHC.Base
import Data.Char
import System.IO
import Data.Time
-- import Data.FiniteMap
-- import Data.Set
--import Control.Monad
--import GHC.IO.Handle
import Numeric
import Control.Concurrent
import Control.Exception as Exception
----  import System.Random

import Lib.Arrow.Runnable
import Lib.Monad.MonadT
import Lib.Monad.MonadIO
import Lib.XML.Types
import Lib.HTML.Types
import Lib.HTML.MonadHtml
import Lib.HTML.HtmlFragmentT
import Lib.HTTP.Types
import Lib.HTTP.Server
-- import Lib.HTTP.Client
import Lib.Server.Types
import Lib.MetaSearch.MetaSearch
import Lib.HTTP.State

data BMState = BMState {
    connectionThread :: Maybe ThreadId,
    stateKey :: String,
    userName :: String,
    searchResults :: [Property],
    searchParameters :: Maybe (String,Int,Int,Int,Int)
}

makeBMState :: BMState
makeBMState = BMState {
    connectionThread = Nothing,
    stateKey = "",
    userName = "",
    searchResults = [],
    searchParameters = Nothing
}

bookamoveHandler :: ContinuationChan BMState -> HttpHandler
bookamoveHandler stateChannel req _ = case uriPath (requestURI req) of
    UriPath u | u=="/" || u=="/index.html" || u=="/index.htm"  -> do
        respondHTML req (\rsp -> run (appMain req rsp stateChannel)) HttpOk
    UriPath "/logout.html" -> do
        deleteState stateChannel (lookupDict (requestURI req) "session")
        respondRedirect req "/" 
    UriPath "/results.csv" -> do
        csv <- do
            key <- return $ lookupDict (requestURI req) "session"
            maybeState <- readState stateChannel key
            case maybeState of
                Just state -> do
                    touchState stateChannel key
                    getCsv state
                _ -> return []
        respondRaw req csv HttpOk
    _ -> respondHTML req (\rsp -> run (htmlNotFound req rsp)) HttpNotFound

khadoma_colour :: String
khadoma_colour = "#336699"

khadomaStyle :: Attribute
khadomaStyle = MkAttribute ("style","font-family:arial,helvetica,sans-serif;")

mySpan :: HtmlFragmentT IO () -> HtmlFragmentT IO ()
mySpan c = attrSpan [MkAttribute ("style","color:#ffffff;background:"++khadoma_colour++";font-family:arial,helvetica,sans-serif;")] $ htmlSmall c

wSpan :: HtmlFragmentT IO () -> HtmlFragmentT IO ()
wSpan c = attrSpan [MkAttribute ("style","font-family:arial,helvetica,sans-serif;")] $ htmlSmall c

isValidLogin :: String -> String -> Bool
isValidLogin user pass 
    | user=="keean" && pass=="test321" = True
    | user=="fryit" && pass=="test123" = True
    | otherwise = False

appMain :: HttpRequest -> Response -> ContinuationChan BMState -> HtmlFragmentT IO ()
appMain req rsp stateChannel = do
    maybeState <- up $ readState stateChannel (lookupDict (requestURI req) "session")
    case maybeState of
        Nothing -> let user = lookupDict (requestURI req) "username"
            in if isValidLogin user (lookupDict (requestURI req) "password")
                then do
                    ioHPutStr stderr $ (showString "Login by " . showString user) ".\n"
                    key <- up $ newKey
                    tid <- up $ myThreadId
                    state <- return $ makeBMState {
                        connectionThread=Just tid,
                        stateKey=key,
                        userName=user}
                    up $ writeState stateChannel key state
                    manager req rsp stateChannel state
                else login rsp
        Just state -> do
            up $ touchState stateChannel (stateKey state)
            manager req rsp stateChannel state

login :: Response -> HtmlFragmentT IO ()
login rsp = do
    htmlDoc $ do
        htmlHead $ htmlTitle "Khadoma.com"
        attrBody [MkAttribute ("topmargin","0"),MkAttribute ("leftmargin","0"),MkAttribute ("bgcolor",khadoma_colour),
                MkAttribute ("marginwidth","0"),MkAttribute ("marginheight","0")] $ htmlForm "post" "" $ do
            attrDiv [MkAttribute ("style","width:"++(showInt tableWidth "")++";margin-left:0px;margin-right:0px;color:#ffffff;background:"++khadoma_colour++";font-family:arial,helvetica,sans-serif;")] $ do
                attrNullImg [MkAttribute ("height","3")]
                htmlBR
                htmlNobr $ do
                    htmlB (htmlNobrText " Khadoma.com ")
                    htmlSmall (htmlNobrText "- login required")
                htmlBR
                attrNullImg [MkAttribute ("height","2")]
                htmlBR
            attrDiv [MkAttribute ("style","width:"++(showInt tableWidth "")++";margin-left:0px;margin-right:0px;color:#ffffff;background:"++khadoma_colour++";font-family:arial,helvetica,sans-serif;")] $ do
                attrTable [MkAttribute ("width",showInt tableWidth ""),MkAttribute ("cols","5")] $ do
                    htmlTR $ do
                        attrTD [MkAttribute ("width","25")] $ attrNullImg [MkAttribute ("width","25")]
                        attrTD [MkAttribute ("width",showInt (tableWidth-50) "")] $
                            attrSpan [MkAttribute ("style","background:#ffffff;")] $
                                attrNullImg [MkAttribute ("width",showInt (tableWidth-50) ""),MkAttribute ("height","1")]
                        attrTD [MkAttribute ("width","25")] $ attrNullImg [MkAttribute ("width","25")]
                    htmlTR $ attrTD [MkAttribute ("width",showInt (tableWidth-50) "")] $ do
                        attrNullImg [MkAttribute ("height","6")]
                attrTable [MkAttribute ("cellspacing","4")] $ do
                    htmlTR $ do
                        attrTD [MkAttribute ("width","25")] $ attrNullImg [MkAttribute ("width","25")]
                        attrTD [MkAttribute ("valign","middle"),MkAttribute ("width","100")] $
                            mySpan $ htmlNobrText "username"
                        attrTD [MkAttribute ("valign","middle")] $
                            mySpan $ do
                                attrTextEdit [MkAttribute ("tabindex","1"),MkAttribute ("size","40")] "username" ""
                        attrTD [MkAttribute ("rowspan","2"),MkAttribute ("valign","middle"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                            htmlNobrText " "
                        attrTD [MkAttribute ("bgcolor","#ffffff"),MkAttribute ("rowspan","2"),MkAttribute ("valign","middle")] $ attrNullImg [MkAttribute ("width","1"),MkAttribute ("height","100%")]
                        attrTD [MkAttribute ("rowspan","2"),MkAttribute ("valign","middle"),MkAttribute ("width","100%")] $ mySpan $ htmlNobr $ do
                            htmlNobrText "  "
                            attrSubmit [MkAttribute ("tabindex","3")] "   Login   "
                        attrTD [MkAttribute ("width","25")] $ attrNullImg [MkAttribute ("width","25")]
                    htmlTR $ do
                        attrTD [MkAttribute ("width","25")] $ attrNullImg [MkAttribute ("width","25")]
                        attrTD [MkAttribute ("valign","middle"),MkAttribute ("width","100")] $
                            mySpan $ htmlNobrText "password"
                        attrTD [MkAttribute ("valign","middle")] $
                            mySpan $ do
                                attrPasswordEdit [MkAttribute ("tabindex","2"),MkAttribute ("size","40")] "password"
                        attrTD [MkAttribute ("width","25")] $ attrNullImg [MkAttribute ("width","25")]
    write rsp
    write rsp

                        

manager :: HttpRequest -> Response -> ContinuationChan BMState -> BMState -> HtmlFragmentT IO ()
manager req rsp stateChannel state = do
    htmlDoc $ do
        htmlHead $ htmlTitle $ "Khadoma.com"
        attrBody [MkAttribute ("link",khadoma_colour),MkAttribute ("vlink","#003366"),
            MkAttribute("alink","#996633")] $ do
                    htmlTable $ do
                        htmlTR $ do
                            htmlTD $ htmlNobrText " "
                            htmlTD $ htmlCenter $ wSpan $ htmlLink ("logout.html?session="++stateKey state)
                                $ htmlNobrText "Logout"
                        htmlTR $ attrTD [MkAttribute ("colspan","2")] $ attrNullImg [MkAttribute ("height","3")]
                        htmlTR $ do
                            htmlTD $ do
                                attrImg [MkAttribute ("hspace","5"),MkAttribute ("vspace","7"),
                                    MkAttribute ("align","left"),MkAttribute ("alt","Khadoma")]
                                    "http://images.khadoma.com/images/khadoma_small.gif"
                            htmlTD $ htmlForm "post" "" $ attrTable [MkAttribute ("cellpadding","1")] $ do
                                    htmlTR $ attrTD [MkAttribute ("bgcolor","#336699")]
                                            $ attrTable [MkAttribute ("cellpadding","3")] $ do
                                        htmlTR $ attrTD [MkAttribute ("bgcolor","white")] $ wSpan $ htmlNobr $ do
                                            htmlSmall $ htmlNobrText "Postcodes "
                                            attrTextEdit [MkAttribute ("tabindex","1"),
                                                MkAttribute ("size","15")] "pcds"
                                                (map toUpper $ lookupDict (requestURI req) "pcds")
                                            htmlSmall $ htmlNobrText "     Price range "
                                            attrTextEdit [MkAttribute ("tabindex","2"),MkAttribute ("align","right"),
                                                MkAttribute ("size","3")] "p0" (lookupDict (requestURI req) "p0")
                                            htmlSmall $ htmlNobrText " K  -  "
                                            attrTextEdit [MkAttribute ("tabindex","3"),MkAttribute ("align","right"),
                                                MkAttribute ("size","3")] "p1" (lookupDict (requestURI req) "p1")
                                            htmlSmall $ htmlNobrText " K     Bedrooms "
                                            attrTextEdit [MkAttribute ("tabindex","4"),MkAttribute ("align","right"),
                                                MkAttribute ("size","3")] "b0" (lookupDict (requestURI req) "b0")
                                            htmlSmall $ htmlNobrText "  -  "
                                            attrTextEdit [MkAttribute ("tabindex","5"),MkAttribute ("align","right"),
                                                MkAttribute ("size","3")] "b1" (lookupDict (requestURI req) "b1")
                                        htmlTR $ attrTD [MkAttribute ("bgcolor","white")] $ wSpan $ htmlNobr $ do
                                            htmlHidden "session" (stateKey state)
                                            htmlCenter $ attrSubmit [MkAttribute ("tabindex","6")] "  Khadoma Search  "
                        htmlTR $ attrTD [MkAttribute ("colspan","2")] $ attrNullImg [MkAttribute ("height","8")]
                    attrTable [MkAttribute ("width","100%")] $ htmlTR $ attrTD [MkAttribute ("bgcolor","336699")]
                        $ htmlNullImg 
                    attrTable [MkAttribute ("width","100%"),MkAttribute ("cellpadding","3")]
                        $ htmlTR $ attrTD [MkAttribute ("bgcolor","ddeeff")] $ wSpan
                        $ case (map toUpper $ lookupDict (requestURI req) "pcds",lookupDict (requestURI req) "p0",
                                lookupDict (requestURI req) "p1",lookupDict (requestURI req) "b0",
                                lookupDict (requestURI req) "b1") of
                            (pcds,p0,p1,b0,b1)
                                | pcds /= "" && p0 /= "" && p1 /= "" && b0 /= "" && b1 /= "" -> case (readDec p0,
                                    readDec p1,readDec b0,readDec b1) of
                                        ([(i0::Integer,_)],[(i1::Integer,_)],[(j0::Integer,_)],[(j1::Integer,_)]) -> htmlNobr
                                            $ htmlNobrText (((showString "Searching for " . showString pcds
                                                . showString ", " . showInt i0 . showString "K - " . showInt i1
                                                . showString "K, " . showInt j0 . showString " - " . showInt j1)
                                                    " (results shown as they arrive)"))
                                        _ -> htmlNobr $ htmlNobrText "Search ranges are not numeric."
                                | otherwise -> case searchParameters state of
                                    Just (pcds,i0,i1,j0,j1) -> htmlNobr
                                        $ htmlNobrText (((showString "Cached search results for " . showString pcds
                                            . showString ", " . showInt i0 . showString "K - " . showInt i1
                                            . showString "K, " . showInt j0 . showString " - " . showInt j1) ""))
                                    _ -> htmlNobr $ htmlNobrText "No searches done."
                    write rsp
                    case (map toUpper $ lookupDict (requestURI req) "pcds",lookupDict (requestURI req) "p0",
                            lookupDict (requestURI req) "p1",lookupDict (requestURI req) "b0",
                            lookupDict (requestURI req) "b1") of
                        (pcds,p0,p1,b0,b1)
                            | pcds /= "" && p0 /= "" && p1 /= "" && b0 /= "" && b1 /= "" -> case (readDec p0,
                                readDec p1,readDec b0,readDec b1) of
                                    ([(i0,_)],[(i1,_)],[(j0,_)],[(j1,_)]) -> do
                                        up $ writeState stateChannel (stateKey state) (state {
                                            searchResults = [],
                                            searchParameters = Just (pcds,i0,i1,j0,j1) })
                                        (ms,_) <- up $ metaSearch pcds (1000*i0) (1000*i1) j0 j1
                                        -- up $ hPutStr stderr $ shows ms "\n"
                                        up $ Exception.catch ((run $ htmlResultsTable rsp state ms) :: IO ()) (\(e::SomeException) -> do
                                            hPutStr stderr $ shows e "\n")
                                        up $ writeState stateChannel (stateKey state) (state {
                                            searchResults = ms,
                                            searchParameters = Just (pcds,i0,i1,j0,j1) })
                                    _ -> return ()
                            | otherwise -> case searchParameters state of
                                Just (pcds,_i0,_i1,_j0,_j1) -> htmlResultsTable rsp state (searchResults state)
                                _ -> return ()
    htmlBR  
    write rsp
    write rsp

                    {-
        attrBody [MkAttribute ("link",khadoma_colour),MkAttribute ("vlink","#003366"),MkAttribute("alink","#996633"),
                MkAttribute ("topmargin","0"),MkAttribute ("leftmargin","0"),
                MkAttribute ("marginwidth","0"),MkAttribute ("marginheight","0")] $ htmlForm "post" "" $ do
                    attrDiv [MkAttribute ("style","width:"++(showInt tableWidth "")++";margin-left:0px;margin-right:0px;color:#ffffff;background:"++khadoma_colour++";font-family:arial,helvetica,sans-serif;")] $ do
                        attrNullImg [MkAttribute ("height","3")]
                        htmlBR
                        htmlNobr $ do
                            htmlB (htmlNobrText " Khadoma.com ")
                            htmlSmall $ do
                                htmlNobrText $ "- " ++ userName state
                        htmlBR
                        attrNullImg [MkAttribute ("height","2")]
                        htmlBR
                    attrDiv [MkAttribute ("style","width:"++(showInt tableWidth "")++";margin-left:0px;margin-right:0px;color:#ffffff;background:"++khadoma_colour++";font-family:arial,helvetica,sans-serif;")] $ do
                        attrTable [MkAttribute ("width",showInt tableWidth ""),MkAttribute ("cols","5")] $ do
                            htmlTR $ do
                                attrTD [MkAttribute ("width","25")] $ attrNullImg [MkAttribute ("width","25")]
                                attrTD [MkAttribute ("width",showInt (tableWidth-50) "")] $
                                    attrSpan [MkAttribute ("style","background:#ffffff;")] $
                                        attrNullImg [MkAttribute ("width",showInt (tableWidth-50) ""),MkAttribute ("height","1")]
                                attrTD [MkAttribute ("width","25")] $ attrNullImg [MkAttribute ("width","25")]
                            htmlTR $ attrTD [MkAttribute ("width",showInt (tableWidth-50) "")] $ do
                                attrNullImg [MkAttribute ("height","6")]
                        attrTable [MkAttribute ("cellspacing","4")] $ do
                            htmlTR $ do
                                attrTD [MkAttribute ("rowspan","2"),MkAttribute ("width","1")] $ htmlSmall $ htmlNobrText "      "
                                attrTD [MkAttribute ("valign","middle"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    htmlNobrText "Postcodes "
                                attrTD [MkAttribute ("valign","middle"),MkAttribute ("colspan","8"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    attrTextEdit [MkAttribute ("tabindex","1"),MkAttribute ("size","75")] "pcds" (map toUpper $ lookupDict (requestURI req) "pcds")
                                attrTD [MkAttribute ("rowspan","2"),MkAttribute ("valign","middle"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    htmlNobrText " "
                                attrTD [MkAttribute ("bgcolor","#ffffff"),MkAttribute ("rowspan","2"),MkAttribute ("valign","middle")] $ attrNullImg [MkAttribute ("width","1"),MkAttribute ("height","100%")]
                                attrTD [MkAttribute ("rowspan","2"),MkAttribute ("valign","middle"),MkAttribute ("width","100%")] $ mySpan $ htmlNobr $ do
                                    htmlNobrText "  "
                                    attrSubmit [MkAttribute ("tabindex","6")] "   New Search   "
                            htmlTR $ do
                                attrTD [MkAttribute ("align","left"),MkAttribute ("valign","middle"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    htmlNobrText "Price range "
                                attrTD [MkAttribute ("valign","middle"),MkAttribute ("align","left"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    attrTextEdit [MkAttribute ("tabindex","2"),MkAttribute ("align","right"),MkAttribute ("size","9")] "p0" (lookupDict (requestURI req) "p0")
                                attrTD [MkAttribute ("valign","middle"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    htmlNobrText " K  - "
                                attrTD [MkAttribute ("valign","middle"),MkAttribute ("align","left"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    attrTextEdit [MkAttribute ("tabindex","3"),MkAttribute ("align","right"),MkAttribute ("size","9")] "p1" (lookupDict (requestURI req) "p1")
                                attrTD [MkAttribute ("valign","middle"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    htmlNobrText " K  "
                                attrTD [MkAttribute ("valign","middle"),MkAttribute ("align","right"),MkAttribute ("width","100%")] $ mySpan $ htmlNobr $ do
                                        htmlNobrText "   Bedrooms "
                                attrTD [MkAttribute ("valign","middle"),MkAttribute ("align","right"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    attrTextEdit [MkAttribute ("tabindex","4"),MkAttribute ("align","right"),MkAttribute ("size","9")] "b0" (lookupDict (requestURI req) "b0")
                                attrTD [MkAttribute ("valign","middle"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    htmlNobrText " - "
                                attrTD [MkAttribute ("valign","middle"),MkAttribute ("align","right"),MkAttribute ("width","1")] $ mySpan $ htmlNobr $ do
                                    attrTextEdit [MkAttribute ("tabindex","5"),MkAttribute ("align","right"),MkAttribute ("size","9")] "b1" (lookupDict (requestURI req) "b1")
                                    htmlHidden "session" (stateKey state)
                        attrNullImg [MkAttribute ("height","8")]
                        htmlBR
                    write rsp
                    attrDiv [MkAttribute ("style","margin-left:10;margin-right:10;")] $ wSpan $ htmlNobr $ do
                        htmlNobrText "["
                        htmlLink ("logout.html?session="++stateKey state) $ htmlNobrText "logout"
                        htmlNobrText "] "
                    case (map toUpper $ lookupDict (requestURI req) "pcds",lookupDict (requestURI req) "p0",
                                lookupDict (requestURI req) "p1",lookupDict (requestURI req) "b0",
                                lookupDict (requestURI req) "b1") of
                            (pcds,p0,p1,b0,b1)
                                | pcds /= "" && p0 /= "" && p1 /= "" && b0 /= "" && b1 /= "" -> case (readDec p0,
                                    readDec p1,readDec b0,readDec b1) of
                                        ([(i0,_)],[(i1,_)],[(j0,_)],[(j1,_)]) -> do
                                            htmlNobr $ htmlNobrText (((showString "Searching for " . showString pcds
                                                . showString ", " . showInt i0 . showString "K - " . showInt i1
                                                . showString "K, " . showInt j0 . showString " - " . showInt j1) " (results shown as they arrive)"))
                                            htmlBR
                                            htmlBR
                                            write rsp
                                            up $ writeState stateChannel (stateKey state) (state {
                                                searchResults = [],
                                                searchParameters = Just (pcds,i0,i1,j0,j1) })
                                            (ms,_) <- up $ metaSearch pcds (1000*i0) (1000*i1) j0 j1
                                            -- up $ hPutStr stderr $ shows ms "\n"
                                            up $ Exception.catch ((run $ htmlResultsTable rsp state ms) :: IO ()) (\e -> do
                                                hPutStr stderr $ shows e "\n")
                                            up $ writeState stateChannel (stateKey state) (state {
                                                searchResults = ms,
                                                searchParameters = Just (pcds,i0,i1,j0,j1) })
                                        _ -> do
                                            attrDiv [MkAttribute ("style","margin-left:10;margin-right:10;")] $ do
                                                wSpan $ htmlNobrText "Search ranges are not numeric."
                                            htmlCurrentResults rsp state
                                | otherwise -> htmlCurrentResults rsp state
    htmlBR  
    write rsp
    write rsp -}

-- throwToList :: [ThreadId] -> Exception -> IO ()
-- throwToList [] _ = return ()
-- throwToList (t0:ts) e = do
--  throwTo t0 e
--  throwToList ts e

htmlCurrentResults :: Response -> BMState ->  HtmlFragmentT IO ()
htmlCurrentResults rsp state = case searchParameters state of
    Just (pcds,i0,i1,j0,j1) -> do
        attrDiv [MkAttribute ("style","margin-left:10;margin-right:10;")] $ do
            wSpan $ htmlNobr $ htmlNobrText (((showString "Cached search results for " . showString pcds
                . showString ", " . showInt i0 . showString "K - " . showInt i1
                . showString "K, " . showInt j0 . showString " - " . showInt j1) ""))
            write rsp
            htmlResultsTable rsp state (searchResults state)
    _ -> do
        attrDiv [MkAttribute ("style","margin-left:10;margin-right:10;")] $ do
            wSpan $ htmlNobr $ htmlNobrText "No searches done."
        write rsp

tableWidth :: Int
tableWidth = (10+30+10+180+10+20+10+40+10+75+10+35+10+90+10+150+10+300+10+300)

htmlResultsTable :: Response -> BMState -> [Property] -> HtmlFragmentT IO ()
htmlResultsTable rsp _ [] = do
    htmlBR
    attrTable [MkAttribute ("width",showInt tableWidth "")] $ htmlTR $ do 
        attrTD [MkAttribute ("width","10")] $ wSpan $ attrNullImg [MkAttribute ("width","10")]
        attrTD [MkAttribute ("width",showInt (tableWidth-10) "")] $ wSpan $ htmlNobrText "[no results in search range]"
    write rsp
htmlResultsTable rsp state ms@(_:_) = do
    {-
    htmlBR
    attrDiv [MkAttribute ("style","width:"++(showInt tableWidth "")++";margin-left:0px;margin-right:0px;color:#ffffff;background:"++khadoma_colour++";font-family:arial,helvetica,sans-serif;")] $ do
        attrTable [MkAttribute ("width",showInt tableWidth "")] $ htmlTR $ do
            attrTD [MkAttribute ("width","10")] $ mySpan $ htmlNullImg
            attrTD [MkAttribute ("width","30")] $ mySpan $ htmlNobrText "No."
            attrTD [MkAttribute ("width","10")] $ mySpan $ htmlNullImg
            attrTD [MkAttribute ("width","180")] $ mySpan $ htmlNobrText "Timestamp"
            attrTD [MkAttribute ("width","10")] $ mySpan $ htmlNullImg
            attrTD [MkAttribute ("width","20")] $ mySpan $ htmlNobrText "Src"
            attrTD [MkAttribute ("width","10")] $ mySpan $ htmlNullImg
            attrTD [MkAttribute ("width","40")] $ mySpan $ htmlNobrText "Area"
            attrTD [MkAttribute ("width","10")] $ mySpan $ htmlNullImg
            attrTD [MkAttribute ("width","75")] $ mySpan $ htmlNobrText "Price"
            attrTD [MkAttribute ("width","10")] $ mySpan $ htmlNullImg
            attrTD [MkAttribute ("width","35")] $ mySpan $ htmlNobrText "Beds"  
            attrTD [MkAttribute ("width","10")] $ mySpan $ htmlNullImg
            attrTD [MkAttribute ("width","90")] $ mySpan $ htmlNobrText "Type"
            attrTD [MkAttribute ("width","10")] $ mySpan $ htmlNullImg
            attrTD [MkAttribute ("width","150")] $ mySpan $ htmlNobrText "Address"
            attrTD [MkAttribute ("width","10")] $ mySpan $ htmlNullImg
            attrTD [MkAttribute ("width","300")] $ mySpan $ htmlNobrText "Description"
            attrTD [MkAttribute ("width","10")] $ mySpan $ htmlNullImg
            attrTD [MkAttribute ("width","300")] $ mySpan $ htmlNobrText "Agent"
    write rsp
    -}
    (c0,c1,c2,c3) <- htmlMetaResults rsp (1,1,1,1) ms state
    htmlBR
    {-
    attrTable [MkAttribute ("width",showInt tableWidth "")] $ htmlTR $ do 
        attrTD [MkAttribute ("width","10")] $ wSpan $ attrNullImg [MkAttribute ("width","10")]
        attrTD [MkAttribute ("width",showInt (tableWidth-10) "")] $ wSpan $ htmlNobr $ do
    -}
    wSpan $ do
        htmlNobrText "["
        htmlLink ("results.csv?session="++stateKey state) $
            htmlNobrText ("download "++show (c0+c1+c2+c3-4)++" properties as csv file ("
                ++show (c0-1)++","++show (c1-1)++","++show (c2-1)++","++show (c3-1)++")")
        htmlNobrText "]"
    write rsp

removeCommas :: String -> String
removeCommas [] = []
removeCommas (c0:[])
    | c0==',' || c0=='\n' || c0=='\r' = []
    | isAscii c0 = [c0]
    | otherwise = []
removeCommas (c0:cs@(c1:_))
    | c0=='\r' = removeCommas cs
    | (c0==',' || c0=='\n') && c1==' ' = removeCommas cs
    | (c0==',' || c0=='\n') = ' ':removeCommas cs
    | isAscii c0 = c0:removeCommas cs
    | otherwise = removeCommas cs

maybeShowString :: Maybe String -> ShowS
maybeShowString (Just s) = showString s
maybeShowString Nothing = id

maybeShowTime :: Maybe UTCTime -> IO ShowS
maybeShowTime (Just t) = do
    c <- utcToLocalZonedTime t
    return $ showString $ show c -- (calendarTimeToString c)
maybeShowTime Nothing = return id

maybeShowInt :: Maybe Int -> ShowS
maybeShowInt (Just i) = showInt i
maybeShowInt Nothing = id

csvMetaResults :: [Property] -> IO ShowS 
csvMetaResults [] = return id
csvMetaResults (p0:ps) = do
    s <- maybeShowTime $ propertyTimestamp p0
    r <- csvMetaResults ps
    return $ showChar '"' . s . showString "\", \""
        . maybeShowString (fmap removeCommas $ propertySite p0) . showString "\", "
        . maybeShowString (fmap removeCommas $ propertyPostcode p0) . showString "\", "
        . maybeShowInt (propertyPrice p0) . showString ", "
        . maybeShowInt (propertyBeds p0) . showString ", \""
        . maybeShowString (fmap removeCommas $ propertyType p0) . showString "\", \"" 
        . maybeShowString (fmap removeCommas $ propertyAddress p0) . showString "\", \"" 
        . maybeShowString (fmap removeCommas $ propertyDescription p0) . showString "\", \"" 
        . maybeShowString (fmap removeCommas $ propertyAgent p0) . showString "\"\n"
        . r

getCsv :: BMState -> IO String
getCsv state = do
    csv <- csvMetaResults (searchResults state)
    return $ csv ""

htmlMetaResults :: Response -> (Int,Int,Int,Int) -> [Property] -> BMState -> HtmlFragmentT IO (Int,Int,Int,Int)
htmlMetaResults _ cnt [] _ = return cnt
htmlMetaResults rsp (c0,c1,c2,c3) (p0:ps) state = attrP [khadomaStyle] $ do
                cnt <- return (c0+c1+c2+c3-3)
                {- attrTable (metaAttr ((cnt `mod` 2)==0)) $ do -}
                {-
                    attrTable (metaColour "#ffffff") $ do
                    htmlTR $ do
                    attrTD [khadomaStyle,MkAttribute ("width","100%")] $ do
                -}
                case propertyImage p0 of
                    Nothing -> attrNullImg [MkAttribute ("align","left"),MkAttribute ("border","0"),
                        MkAttribute ("alt","no image"),MkAttribute ("width","81"),
                        MkAttribute ("height","61")] -- 75 x 56
                    Just l -> attrImg [MkAttribute ("align","left"),
                        MkAttribute ("width","81"),MkAttribute ("height","61")] l
                case propertyDetails p0 of
                    Nothing -> attrFont [MkAttribute ("color",khadoma_colour)] $ do
                        htmlText [CharData (maybeShowString (propertyAddress p0) "")]
                    Just l -> case searchParameters state of
                         Just (pcds,_,_,_,_) -> htmlLink l $ intoTitle (toWords pcds)
                            . toWords $ maybeShowString (propertyAddress p0) ""
                         _ -> htmlLink l $ intoTitle (toWords "")
                            . toWords $ maybeShowString (propertyAddress p0) ""
                htmlNobrText " - "
                attrFont [MkAttribute ("color","#996633")] $ do
                    htmlSmall $ htmlNobr $ htmlText [EntityRef "pound",CharData (maybeShowInt (propertyPrice p0) "")]
                htmlNobrText " - "
                attrFont [MkAttribute ("color","#996633")] $ do
                    htmlSmall $ htmlNobrText ((maybeShowInt (propertyBeds p0) . showString " beds") "")
                case propertyType p0 of
                    Nothing -> htmlNobrText " "
                    Just p -> htmlSmall $ do
                        htmlNobrText " - "
                        attrFont [MkAttribute ("color","#996633")] $ do
                            htmlNobrText (showString p "")
                        htmlNobrText " "
                {- htmlNobrText (maybeShowString (propertyPostcode p0) " ") -}
                htmlBR
                htmlSmall $ do
                    htmlText [CharData (maybeShowString (propertyDescription p0) " ")]
                    htmlBR
                    case propertySite p0 of
                        Just "PL" -> do
                            attrFont [MkAttribute ("color","#996633")] $ htmlNobrText "www.primelocation.com" 
                            htmlNobrText " - "
                        Just "RM" -> do
                            attrFont [MkAttribute ("color","#996633")] $ htmlNobrText "www.rightmove.co.uk"
                            htmlNobrText " - "
                        Just "PF" -> do
                            attrFont [MkAttribute ("color","#996633")] $ htmlNobrText "www.propertyfinder.co.uk"
                            htmlNobrText " - "
                        Just "PP" -> do
                            attrFont [MkAttribute ("color","#996633")] $ htmlNobrText "www.propertyfinder.co.uk"
                            htmlNobrText " - "
                        _ -> do return ()
                    case propertyAgentLink p0 of
                        Just l -> attrLink l [MkAttribute ("style","color:#6699cc")] $ htmlNobrText (maybeShowString (propertyAgent p0) "")
                        _ -> attrFont [MkAttribute ("color","#996633")] $ htmlNobrText (maybeShowString (propertyAgent p0) " ")
                attrBR [MkAttribute ("clear","all")]
                -- htmlBR
                write rsp
                cnt' <- return $ case propertySite p0 of
                    Just "PL" -> (c0+1,c1,c2,c3)
                    Just "RM" -> (c0,c1+1,c2,c3)
                    Just "PP" -> (c0,c1,c2+1,c3)
                    Just "PF" -> (c0,c1,c2,c3+1)
                    _ -> (c0,c1,c2,c3)
                htmlMetaResults rsp cnt' ps state

metaAttr :: Bool -> [Attribute]
metaAttr a = if a
    then metaColour "#ffefdf"
    else metaColour "#ffdfcf"

metaColour :: String -> [Attribute]
metaColour c = [MkAttribute ("border","0"),MkAttribute ("bgcolor",c),MkAttribute ("width","800"),MkAttribute ("color","#000000")]
            
getHandler :: IO ConnectionHandler
getHandler = do
    cmdChannel <- getContinuationChan
    return $ httpHandler (bookamoveHandler cmdChannel)

toWords :: String -> [String]
toWords s = toWords' "" s

toWords' :: String -> String -> [String]
toWords' ac (c0:cs)
    | isAlpha c0 = (reverse ac):toWords'' [c0] cs
    | otherwise = toWords' (c0:ac) cs
toWords' ac _ = [reverse ac]

toWords'' :: String -> String -> [String]
toWords'' ac (c0:cs) 
    | isAlphaNum c0 = toWords'' (c0:ac) cs
    | otherwise = (reverse ac):toWords' [c0] cs
toWords'' ac _ = [reverse ac]

intoTitle :: MonadHtml m => [String] -> [String] -> m () 
intoTitle wl (s0:ss)
    | s0 `wordIn` wl = do
        htmlB $ case foldr (\c b -> b && isAlpha c) True s0 of
            True -> htmlNobrText $ fmTw s0
            False -> htmlNobrText (map toUpper s0)
        intoTitle wl ss
    | otherwise = do
        case foldr (\c b -> b && isAlpha c) True s0 of
            True -> htmlNobrText $ fmTw s0
            False -> htmlNobrText (map toUpper s0)
        intoTitle wl ss
intoTitle _ _ = return()

fmTw :: String -> String
fmTw (s0:ss) = toUpper s0:map toLower ss
fmTw _ = ""

wordIn :: String -> [String] -> Bool
wordIn s0 (w0:ws)
    | (map toLower w0) == (map toLower s0) = True
    | otherwise = s0 `wordIn` ws
wordIn _ _ = False

