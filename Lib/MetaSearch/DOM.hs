{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- dom.hs (C)2001 Keean Schupke
--
--      DOM parser

module Lib.MetaSearch.DOM(ElemType(..),Elem(..),Attribute(..),DOM,ShowDOM,TransDOM,createDOM,dumpDOM,elemIs,textContains,
    generateXML,extractText,parseStringToDOM,commentContains,elemHasAttribute,hPutXML,parseHeaders,
    attributeContains,getAttributes,getAttributeAsString,elemAttributeIs,extractString,setAttributes,
    moveRoot,domHeader,domNull,domText,domTag,domAttrTag,domJoin,domChild,domMakePage,domNobrText,getLink,
    getLinkWithText) where

import Data.Char
import Control.Monad
import System.IO
import Control.DeepSeq
import Control.DeepSeq.Generics
import GHC.Generics

import Lib.MetaSearch.Parser


data ElemType = Document|Header|Tag|Text|Comment|DTD|Close|EmptyTag|Null deriving (Generic, Eq, Show)
newtype Attribute = MkAttribute (String,String) deriving (Generic, Show)
data Elem = MkElem !(ElemType,String,[Attribute]) deriving (Generic, Show)

instance NFData ElemType where rnf = genericRnf
instance NFData Attribute where rnf = genericRnf
instance NFData Elem where rnf = genericRnf

type DOM = [(Int,Elem)]
type ShowDOM = DOM -> DOM
type TransDOM a = ShowDOM -> (a,ShowDOM)
-- instance Show ElemType where
--  showsPrec _ Document = showString "Document"
--  showsPrec _ Header = showString "Header"
--  showsPrec _ Tag = showString "Tag"
--  showsPrec _ Text = showString "Text"
--  showsPrec _ Comment = showString "Comment"
--  showsPrec _ DTD = showString "DTD"
--  showsPrec _ Close = showString "Close"
--  showsPrec _ EmptyTag = showString "EmptyTag"
--  showsPrec _ Null = showString "Null"

-- token types
{-# INLINE isColon #-}
isColon :: Char -> Bool
isColon = (==':')
{-# INLINE isSlash #-}
isSlash :: Char -> Bool
isSlash = (=='/')
{-# INLINE isStartTag #-}
isStartTag :: Char -> Bool
isStartTag = (=='<')
{-# INLINE isEndTag #-}
isEndTag :: Char -> Bool
isEndTag = (=='>')
{-# INLINE isEqual #-}
isEqual :: Char -> Bool
isEqual = (=='=')
{-# INLINE isQuote #-}
isQuote :: Char -> Bool
isQuote = (=='"') --" fix for a2ps
{-# INLINE isQuote2 #-}
isQuote2 :: Char -> Bool
isQuote2 = (=='\'') --' fix for a2ps
{-# INLINE isPling #-}
isPling :: Char -> Bool
isPling = (=='!')
{-# INLINE isDash #-}
isDash :: Char -> Bool
isDash = (=='-')
--{-# INLINE isReturn #-}
--isReturn :: Char -> Bool
--isReturn = (=='\r')
{-# INLINE isNewline #-}
isNewline :: Char -> Bool
isNewline = (=='\n')
{-# INLINE isNameT #-} -- chars which terminate a name
isNameT :: Char -> Bool
isNameT c = (isValT c) || (isEqual c) || (isSlash c)
{-# INLINE isValT #-} -- chars which terminate value
isValT :: Char -> Bool
isValT c = (isSpace c) || (isEndTag c)
{-# INLINE isScriptT #-} -- chars which (may) terminate a script
isScriptT :: Char -> Bool
isScriptT c = (isSlash c) || (isStartTag c)
--{-# INLINE isHeaderValT #-}
--isHeaderValT :: Char -> Bool
--isHeaderValT c = (isReturn c) || (isNewline c)
--{-# INLINE isHeaderNameT #-}
--isHeaderNameT :: Char -> Bool
--isHeaderNameT c = (isHeaderValT c) || (isColon c) 

-- tag bindings >= 1000 cannot have children
-- {-# INLINE binding #-}
-- binding :: String -> Int
-- binding s = case s of
    -- tags starting with ! cannot be found
--  "!DOC"          -> -1       -- cannot be closed by another tag
--  "HTML"          -> 0        
--  "HEAD"          -> 1
--  "BODY"          ->  1
--  "FORM"          -> 2
--  "DIV"               -> 2
--  "TABLE"         -> 2
--  "TH"                -> 3
--  "TR"                ->  3
--  "TD"                -> 4
--  "FONT"          -> 5
--  "SMALL"         -> 6
--  "BIG"               -> 6
--  "SELECT"            -> 7
--  "B"             -> 9
--  "A"             -> 10
--  "MAP"               -> 20
--  "OPTION"            -> 100  -- self closing (binding>99) tags
--  "LI"                -> 101
--  "P"             -> 1000 -- auto closing (binding>999) tags
--  "BR"                -> 1000
--  "HR"                -> 1000
--  "IMG"               -> 1000
--  "INPUT"         -> 1000
--  "META"          -> 1000
--  "LINK"          -> 1000
--  "AREA"          -> 1000
--  _                   -> 99       -- never closes another tag

-- Terminal tags never have children
isTerminal :: String -> Bool
isTerminal s = case s of
    "BR" -> True
    "HR" -> True
    "IMG" -> True
    "INPUT" -> True
    "META" -> True
    "LINK" -> True
    "AREA" -> True
    "PARAM" -> True
    "HS" -> True
    _ -> False

-- An outside tag closes all tags inside when closed
isCloseOnExit :: String -> String -> Bool
isCloseOnExit s t = case (s,t) of
    ("HTML",_) -> True
    ("HEAD",a) | a /= "HTML" -> True
    ("BODY",a) | a /= "HTML" -> True
    ("TABLE","TR") -> True
    ("TABLE","TD") -> True
    ("TABLE","TH") -> True
    ("TR","TD") -> True
    ("TD","A") -> True
    ("TD","DIV") -> True
    ("TD","FONT") -> True
    ("TD","OPTION") -> True
    ("TD","SELECT") -> True
    ("TD","SPAN") -> True
    ("TD","B") -> True
    ("TD","P") -> True
    ("DIV","FONT") -> True
    ("DIV","TD") -> True
    ("DIV","TR") -> True
    ("DIV","TABLE") -> True
    ("SELECT","OPTION") -> True
    _ -> False

isCloseOnEntry :: String -> String -> Bool
isCloseOnEntry s t = case (s,t) of
    ("P","P") -> True
    ("TR","TD") -> True
    ("TR","TR") -> True
    ("TD","TD") -> True
    ("BODY","HEAD") -> True
    ("OPTION","OPTION") -> True
    ("LI","LI") -> True
    ("TR","SPAN") -> True
    ("TR","FONT") -> True
    ("TD","SPAN") -> True
    ("TD","FONT") -> True
    ("FORM","FORM") -> True
    _ -> False

-- component parsers for HTML -----------------------------

-- parse a quoted string
{-# INLINE quoted #-}
quoted :: (Char -> Bool) -> Parser String
quoted qc = do
    _ <- satisfy qc
    a <- untilChar qc
    _ <- satisfy qc
    return (xmlNormalise a)

-- parse an unquoted string
{-# INLINE unquotedName #-}
unquotedName :: Parser String
unquotedName  = do
    a <- (required . untilChar) isNameT
    return (map toLower a)

{-# INLINE unquotedValue #-}
unquotedValue :: Parser String
unquotedValue = do
    a <- (required . untilChar) isValT
    return (xmlNormalise a)

{-# INLINE tidyName #-}
tidyName :: String -> String
tidyName [] = []
tidyName (c0:cs)
    | c0=='\'' || c0=='`' || c0=='"' = tidyName cs
    | otherwise = c0:tidyName cs

-- parse an attribute name or value
{-# INLINE name #-}
name :: Parser String
name  = do
    a <- (((quoted isQuote) `mplus` (quoted isQuote2)) `mplus` unquotedName)
    return (tidyName a)

{-# INLINE value #-}
value :: Parser String
value = do
    a <- (((quoted isQuote) `mplus` (quoted isQuote2)) `mplus` unquotedValue)
    return a

-- parse an attribute value
{-# INLINE tagValue #-}
tagValue :: Parser String
tagValue  = do
    _ <- (skipSpace . required . whileChar) isEqual
    a <- skipSpace value
    return a
    
-- parse a single attribute
{-# INLINE tagArg #-}
tagArg :: Parser Attribute
tagArg  = do
    a <- skipSpace name
    b <- (tagValue `mplus` return "")
    return (MkAttribute (a,b))

-- parse a start tag 
{-# INLINE _startTag #-}
_startTag :: Parser Elem
_startTag  = do
    a <- skipSpace name
    b <- many tagArg
    c <- (skipSpace . whileChar) isSlash
    _ <- untilChar isEndTag
    _ <- (required . whileChar) isEndTag
    return (case c of
        (_:_) ->(MkElem (EmptyTag,(map toUpper a),b))
        _ -> (MkElem (Tag,(map toUpper a),b)))

-- parse an end tag
{-# INLINE _endTag #-}
_endTag :: Parser Elem
_endTag  = do
    _ <- (skipSpace . required . whileChar) isSlash
    a <- name
    _ <- untilChar isEndTag
    _ <- (required .  whileChar) isEndTag
    return (MkElem (Close,(map toUpper a),[]))

--{-# INLINE startSlash #-}
--startSlash :: Parser ()
--startSlash = do
--  (skipSpace . required . whileChar) isStartTag
--  (skipSpace . required . whileChar) isSlash
--  return ()

--{-# INLINE endTag #-}
--endTag :: Parser Elem
--endTag = do
--  tryParser startSlash
--  a <- skipSpace name
--  (required . whileChar) isEndTag
--  return (MkElem (Close,(map toUpper a),[]))

-- parse a DTD tag
{-# INLINE _dtdTag #-}
_dtdTag :: Parser Elem
_dtdTag  = do
    a <- skipSpace name
    b <- many tagArg
    _ <- (skipSpace . required . whileChar) isEndTag
    return (MkElem (DTD,(map toUpper a),b))

-- parse an end-of-comment
{-# INLINE endComment #-}
endComment :: Parser ()
endComment  = do
    _ <- satisfy isDash
    _ <- satisfy isDash
    _ <- satisfy isEndTag
    return ()

-- parse a comment
{-# INLINE _comment #-}
_comment :: Parser Elem
_comment  =(do
    _ <- satisfy isDash
    _ <- satisfy isDash 
    a <- untilParserOrEnd endComment char
    endComment
    return (MkElem (Comment,format a,[]))) where

    format :: String -> String
    format [] = []
    format (c0:cs)
        | c0=='\r' = format cs
        | otherwise = c0:format cs

-- parse text between tags
{-# INLINE text #-}
text :: Parser Elem
text  = do
    a <- untilChar isStartTag
    return (MkElem (Text,xmlNormalise a,[]))

{-# INLINE _metaTag #-}
_metaTag :: Parser Elem
_metaTag = do
    _ <- (skipSpace . required . whileChar) isPling
    a <- (_comment `mplus` _dtdTag)
    return a

{-# INLINE parseTag #-}
parseTag :: Parser Elem
parseTag = do
    _ <- (skipSpace . required . whileChar) isStartTag
    a <- (_endTag `mplus` _metaTag `mplus` _startTag)
    return a

-- parse any valid elem
{-# INLINE parseElem #-}
parseElem :: Parser Elem
parseElem  = do
    a <- (parseTag `mplus` text)
    return a

-- component parsers for scripts --------------------------

-- test if tag is </SCRIPT>
{-# INLINE scriptEnd #-}
scriptEnd :: Parser ()
scriptEnd = do
    _ <- (skipSpace . required . whileChar) isStartTag
    _ <- (skipSpace . required . whileChar) isSlash
    stringNcs "SCRIPT"
    _ <- (required . whileChar) isEndTag
    return ()

-- parse a script comment started with //
{-# INLINE scriptCommentStart #-}
scriptCommentStart :: Parser ()
scriptCommentStart = do
    _ <- satisfy isSlash
    _ <- satisfy isSlash
    return ()

{-# INLINE scriptComment #-}
scriptComment :: Parser String
scriptComment  = do
    tryParser scriptCommentStart
    a <- untilChar isNewline
    return ('/':('/':a))

-- parser combinators for HTML ----------------------------

findDepth :: Int -> [String] -> String -> Int
findDepth i k s = case k of
    (k0:k') -> if s==k0
        then i-1
        else if isCloseOnExit s k0
        -- else if binding s < binding k0
            then findDepth (i-1) k' s
            else -1
    _ -> -1

upTree :: Int -> Int -> [String] -> [String]
upTree i j k = if i==j
    then k
    else case k of
        (_:k') -> upTree (i-1) j k'
        _ -> []

{-# INLINE bubble #-}
bubble :: Int -> [String] -> String -> (Int,[String])
bubble i k s = case findDepth i k s of
    j   | j < 0 -> (i,k)
        | otherwise -> (j,(upTree i j k))

{-# INLINE bindDOM #-}
bindDOM :: (Int,[String]) -> Elem -> String -> DOM
bindDOM ik@(i,k) e@(MkElem (Tag,n,_)) z = (i,e):(if isTerminal n -- if binding n >= 1000 
    then buildDOM ik z
    else buildDOM ((i+1),n:k) z)
bindDOM ik@(i,_) _ z = (i,(MkElem (Comment,"Attempt to bind non tag element",[]))):buildDOM ik z

buildDOM :: (Int,[String]) -> String -> DOM
buildDOM ik@(i,k) z = case parse parseElem z of
    Consumed result -> case result of
        Ok e@(MkElem (Close,n,_)) z' -> (i,e):buildDOM (bubble i k n) z'
        Ok (MkElem (EmptyTag,n,a)) z' -> (i,MkElem (Tag,n,a)):buildDOM ik z'
        Ok e@(MkElem (Tag,"SCRIPT",_)) z' -> (i,e):buildScript (i+1,"SCRIPT":k) z'
        Ok e@(MkElem (Tag,n,_)) z' -> case k of
            (k0:_)
                | isCloseOnEntry n k0 -> (i-1,e):buildDOM ik z'
                | isTerminal k0 -> (i-1,e):buildDOM ik z'
                | otherwise -> bindDOM ik e z'
            _ -> bindDOM ik e z'
                -- | binding k0 >= 100 -> if k0==n
                --  then (i-1,e):buildDOM ik z'
                --  else bindDOM ik e z' 
                -- | otherwise -> bindDOM ik e z'
            -- _ -> bindDOM ik e z'
        Ok e z' -> (i,e):buildDOM ik z' 
        _ -> []
    _ -> [] 

{-# INLINE parseHeader #-}
parseHeader :: Parser (String,String)
parseHeader = do
    a <- (skipBlanks . required . untilChar) (\c -> c==':' || c=='\n' || c=='\r')
    _ <- (skipBlanks . whileChar) isColon
    b <- (skipBlanks . untilChar) (\c -> c=='\n' || c=='\r')
    _ <- (skipBlanks . satisfy) isNewline
    return (a,b)

{-# INLINE parseHeaders #-}
parseHeaders :: Parser [(String,String)]
parseHeaders = untilParserOrEnd parseBlank parseHeader

{-# INLINE parseBlank #-}
parseBlank :: Parser ()
parseBlank = do
    _ <- whileChar isBlank
    _ <- (required . satisfy) isNewline
    return ()

{-# INLINE createDOM #-}
createDOM :: String -> DOM
createDOM z = case parse parseHeaders z of
    Consumed result -> case result of
        Ok as z' -> (0,MkElem (Header,"",map (\a -> MkAttribute a) as)):buildDOM (0,[]) z'
        _ -> []
    _ -> buildDOM (0,[]) z

{-# INLINE scriptNext #-}
scriptNext :: Parser String
scriptNext = do
    a <- char
    b <- untilChar isScriptT
    return (a:b)

-- note: we cannot finish in a script - always try to find more tags
{-# INLINE buildScript #-}
buildScript :: (Int,[String]) -> String -> DOM
buildScript (i,k) z = case parseResult (untilParserOrEnd scriptEnd (scriptComment `mplus` scriptNext)) z of
        Ok s z' -> case k of
            (_:_) -> (i,(MkElem (Text,concat s,[])))
                        : (i,(MkElem (Close,"SCRIPT",[])))
                        : buildDOM (bubble i k "SCRIPT") z'
            _ -> buildDOM (i,k) z'
        _ -> case k of
            (_:_) -> (i,(MkElem (Comment,"Script Parser Failed",[])))
                        : buildDOM (bubble i k "SCRIPT") z
            _ -> buildDOM (i,k) z

-- DOM manipulation fuctions ------------------------------

xmlNormalise :: String -> String
xmlNormalise ('&':'q':'u':'o':'t':';':cs) = '"':xmlNormalise cs
xmlNormalise ('&':'a':'p':'o':'s':';':cs) = '\'':xmlNormalise cs
xmlNormalise ('&':'a':'m':'p':';':cs) = '&':xmlNormalise cs
xmlNormalise ('&':'l':'t':';':cs) = '&':xmlNormalise cs
xmlNormalise ('&':'g':'t':';':cs) = '&':xmlNormalise cs
xmlNormalise (c0:cs) = c0:xmlNormalise cs
xmlNormalise _ = []

xmlEscape :: String -> String
xmlEscape [] = []
xmlEscape (c0:cs) 
    | c0 == '"' = '&':'q':'u':'o':'t':';':xmlEscape cs
    | c0 == '\'' = '&':'a':'p':'o':'s':';':xmlEscape cs
    | c0 == '&' = '&':'a':'m':'p':';':xmlEscape cs
    | c0 == '<' = '&':'l':'t':';':xmlEscape cs
    | c0 == '>' = '&':'g':'t':';':xmlEscape cs
    | otherwise = c0:xmlEscape cs

{-# INLINE dumpElem #-}
dumpElem  :: Elem -> IO ()
-- dumpElem (MkCode _) = putStr "[IO fn]"
dumpElem (MkElem (t,n,_)) = do
    if t == Header
        then putStr (trimText n)
        else do
            putChar '<'
            case t of
                Document    -> putStr "Document \""
                Header  -> putStr "Header \""
                Tag     -> putStr "Tag \""
                Text        -> putStr "Text \""
                Comment -> putStr "Comment \""
                DTD     -> putStr "DTD \""
                Close       -> putStr "Close \""
                EmptyTag -> putStr "Empty-Tag \""
                Null        -> putStr "Null \""
            putStr (trimText n)
            putStr "\">"

{-# INLINE hPutXML #-}
hPutXML :: Handle -> DOM -> IO ()
hPutXML h dom = do
    --dumpDOM dom
    _generateXML [] dom where

    -- convert Tree into XML representation
    _generateXML :: [(Int,Elem)] -> DOM -> IO ()
    _generateXML [] [] = do
        hPutChar h '\n'
        hFlush h
    _generateXML ((_,o):q') [] = do
        generateTagEnd o
        _generateXML q' []
    _generateXML [] ((j,e):d') = case e of
        -- (MkCode part) -> do
        --  part
        --  _generateXML [] d'
        (MkElem (Tag,_,_)) -> case d' of
            ((j',_):_) -> if j'>j
                then do
                    generateTagStart e
                    _generateXML [(j,e)] d'
                else do
                    generateTag e
                    _generateXML [] d'
            _ -> do
                generateTag e
                _generateXML [] d'
        _ -> do
            generateOther e
            _generateXML [] d'
    _generateXML q@((i,o):q') d@((j,e):d') = case e of
        -- (MkCode part) -> do
        --  if j>i
        --      then do
        --          part
        --          _generateXML q d'
        --      else do
        --          generateTagEnd o
        --          _generateXML q' d
        (MkElem (Tag,_,_)) -> case d' of
            ((j',_):_) -> if j'>j
                then if j>i
                    then do
                        generateTagStart e
                        _generateXML ((j,e):q) d'
                    else do
                        generateTagEnd o
                        _generateXML q' d
                else if j>i
                    then do
                        generateTag e
                        _generateXML q d'
                    else do
                        generateTagEnd o
                        _generateXML q' d
            _ -> if j>i
                then do
                    generateTag e
                    _generateXML q d'
                else do
                    generateTagEnd o
                    _generateXML q' d
        _ -> if j>i 
            then do
                generateOther e
                _generateXML q d'
            else do
                generateTagEnd o
                _generateXML q' d

    generateAttributes :: [Attribute] -> IO ()
    generateAttributes [] = return ()
    generateAttributes ((MkAttribute (n,v)):a') = if n==""
        then do
            hPutStr h (' ':v)
            generateAttributes a'
        else do
            hPutStr h (' ':n)
            hPutStr h ('=':'"':xmlEscape v)
            hPutChar h '"'
            generateAttributes a'

    {-# INLINE generateTagStart #-}
    generateTagStart :: Elem -> IO ()
    generateTagStart (MkElem (Tag,t,a)) = do
        hPutStr h ('<':t)
        generateAttributes a
        hPutChar h '>'
    generateTagStart _ = return ()

    {-# INLINE generateTag #-}
    generateTag :: Elem -> IO ()
    generateTag (MkElem (Tag,t,a)) = do
        hPutStr h ('<':t)
        generateAttributes a
        hPutStr h " />"
    generateTag _ = return ()

    {-# INLINE generateTagEnd #-}
    generateTagEnd :: Elem -> IO ()
    generateTagEnd (MkElem (Tag,t,_)) = do
        hPutStr h "</"
        hPutStr h t
        hPutChar h '>'
    generateTagEnd _ = return ()

    {-# INLINE generateOther #-}
    generateOther :: Elem -> IO ()
    -- generateOther (MkCode _) = hPutStr h "[IO fn]"
    generateOther (MkElem (t,n,a)) = case t of
        Document -> do
            hPutStr h "<DOCUMENT \""
            hPutStr h n
            hPutChar h '\"'
            generateAttributes a
            hPutChar h '>'
        Text -> hPutStr h (xmlEscape n)
        Comment -> do
            hPutStr h "<!--"
            hPutStr h n
            hPutStr h "-->"
        DTD -> do
            hPutStr h "<!"
            hPutStr h n
            generateAttributes a
            hPutChar h '>'
        Header -> do
            generateHeaders a
            hPutChar h '\n'
        _ -> return ()

    generateHeaders :: [Attribute] -> IO ()
    generateHeaders [] = return ()
    generateHeaders ((MkAttribute (n,v)):a') = do
        hPutStr h n
        hPutStr h ": "
        hPutStr h v
        hPutChar h '\n'
        generateHeaders a'

{-# INLINE generateXML #-}
generateXML :: DOM -> String
generateXML dom = _generateXML [] dom where

    -- convert Tree into XML representation
    _generateXML :: [(Int,Elem)] -> DOM -> String
    _generateXML [] [] = ""
    _generateXML ((i,o):q') [] = generateTagEnd i o ++ _generateXML q' []
    _generateXML [] ((j,e):d') = case e of
        (MkElem (Tag,_,_)) -> case d' of
            ((j',_):_) -> if j'>j
                then generateTagStart j e ++ _generateXML [(j,e)] d'
                else generateTag j e ++ _generateXML [] d'
            _ -> generateTag j e ++ _generateXML [] d'
        _ -> generateOther j e ++ _generateXML [] d'
    _generateXML q@((i,o):q') d@((j,e):d') = case e of
        (MkElem (Tag,_,_)) -> case d' of
            ((j',_):_) -> if j'>j
                then if j>i
                    then generateTagStart j e ++ _generateXML ((j,e):q) d'
                    else generateTagEnd i o ++ _generateXML q' d
                else if j>i
                    then generateTag j e ++ _generateXML q d'
                    else generateTagEnd i o ++ _generateXML q' d
            _ -> if j>i
                then generateTag j e ++ _generateXML q d'
                else generateTagEnd i o ++ _generateXML q' d
        _ -> if j>i 
            then generateOther j e ++ _generateXML q d'
            else generateTagEnd i o ++ _generateXML q' d

    generateAttributes :: [Attribute] -> String
    generateAttributes [] = []
    generateAttributes ((MkAttribute (n,v)):a') = if n==""
        then (' ':v)++generateAttributes a'
        else (' ':n)++"=\""++v++('\"':generateAttributes a')

    {-# INLINE generateTagStart #-}
    generateTagStart :: Int -> Elem -> String
    generateTagStart i (MkElem (Tag,t,a)) = (indentSpc i)++('<':t)++generateAttributes a++">\n"
    generateTagStart _ _ = ""

    {-# INLINE generateTag #-}
    generateTag :: Int -> Elem -> String
    generateTag i (MkElem (Tag,t,a)) = (indentSpc i)++('<':t)++generateAttributes a++" />\n"
    generateTag _ _ = ""

    {-# INLINE generateTagEnd #-}
    generateTagEnd :: Int -> Elem -> String
    generateTagEnd i (MkElem (Tag,t,_)) = (indentSpc i)++"</"++t++">\n"
    generateTagEnd _ _ = ""

    {-# INLINE generateOther #-}
    generateOther   :: Int -> Elem -> String
    -- generateOther _ (MkCode _) = ""
    generateOther i (MkElem (t,n,a)) = case t of
        Document -> (indentSpc i)++"<DOCUMENT \""++n++('\"':generateAttributes a)++">\n"
        Text -> (indentSpc i)++n++"\n"
        Comment -> (indentSpc i)++"<!--"++n++"-->\n"
        DTD -> (indentSpc i)++"<!"++n++generateAttributes a++">\n"
        Header -> generateHeaders i a++"\n"
        _ -> ""

    generateHeaders :: Int -> [Attribute] -> String
    generateHeaders _ [] = []
    generateHeaders i ((MkAttribute (n,v)):a') = (indentSpc i)++n++": "++v++('\n':generateHeaders i a')

{-# INLINE indent #-}
indent  :: Char -> Int -> String
indent c i = if i>0 then c:(indent c $! (i-1)) else ""

{-# INLINE indentSpc #-}
indentSpc  :: Int -> String
indentSpc = indent ' '

{-
-- dump tree data - mainly for debugging
dumpElems :: [Elem] -> IO ()
dumpElems [] = putStr ""
dumpElems (e:es) = do
    dumpElem e
    putChar '\n'
    dumpElems es
-}

dumpDOM :: DOM -> IO ()
dumpDOM [] = putStr ""
dumpDOM ((i,e):r) = do
    putStr (indent '|' i)
    putChar '+'
    dumpElem e
    putChar '\n'
    dumpDOM r

-- get list of values for an attribute name
getAttributeAsString :: String -> [Attribute] -> String
getAttributeAsString _ []  = ""
getAttributeAsString s ((MkAttribute (n,v)):a') = if n==s then v else getAttributeAsString s a'

-- get list of values for an attribute name
getAttributes :: String -> DOM -> DOM
getAttributes _ [] = []
getAttributes s ((_,MkElem (Tag,_,a)):es) =
    (concat (map (\(MkAttribute (n,v)) -> if n==s then [(0,MkElem (Text,v,[]))] else []) a))++getAttributes s es
getAttributes s ((_,_):es) = getAttributes s es

{-# INLINE attributesContain #-}
attributesContain :: [Attribute] -> [Attribute] -> Bool
attributesContain [] _ = True
attributesContain (MkAttribute (n0,v0):as) b = if getAttributeAsString n0 b == v0
    then attributesContain as b 
    else False


{-# INLINE elemIs #-}
elemIs :: Elem -> (Elem -> Bool)
elemIs (MkElem (t,n,a)) = (\(MkElem (t',n',a')) -> (t==t') && (n==n') && (attributesContain a a'))
-- elemIs (MkCode _) = (\_ -> False)

_elemHasAttribute :: String -> [Attribute] -> Bool
_elemHasAttribute _ [] = False
_elemHasAttribute s (MkAttribute (n,_):a') = if s == n
    then True
    else _elemHasAttribute s a'

{-# INLINE elemHasAttribute #-}
elemHasAttribute :: String -> (Elem -> Bool)
elemHasAttribute s = (\e -> case e of
    (MkElem (Tag,_,a)) -> _elemHasAttribute (map toLower s) a
    _ -> False)

_attributeContains :: Parser a -> [Attribute] -> Bool
_attributeContains _ [] = False
_attributeContains p (MkAttribute (_,v):a') = case parse (matchParser p char) v of
    Consumed (Ok _ _) -> True
    _ -> _attributeContains p a'

{-# INLINE attributeContains #-}
attributeContains :: Parser a -> (Elem -> Bool)
attributeContains p = (\e -> case e of
    (MkElem (Tag,_,a)) -> _attributeContains p a
    _ -> False)

_attributeIs :: Attribute -> [Attribute] -> Bool
_attributeIs _ [] = False
_attributeIs a@(MkAttribute (n,v)) (MkAttribute (n',v'):b') = if n==n' && v==v'
    then True
    else _attributeIs a b'

{-# INLINE elemAttributeIs #-}
elemAttributeIs :: String -> Attribute -> (Elem -> Bool)
elemAttributeIs n a = (\e -> case e of
    (MkElem (Tag,m,b)) -> if n==m
        then _attributeIs a b
        else False
    _ -> False)

{-# INLINE textContains #-}
textContains :: Parser a -> (Elem -> Bool)
textContains p = (\e -> case e of
    (MkElem (Text,t,_)) -> case parse (matchParser p char) t of
        Consumed (Ok _ _) -> True
        _ -> False
    _ -> False)

{-# INLINE commentContains #-}
commentContains :: Parser a -> (Elem -> Bool)
commentContains p = (\e -> case e of
    (MkElem (Comment,t,_)) -> case parse (matchParser p char) t of
        Consumed (Ok _ _) -> True
        _ -> False
    _ -> False)

-- concat all text elements
extractString :: DOM -> String
extractString d = case d of
    ((_,(MkElem (Text,t,_))):d') -> case extractString d' of
        ts@(_:_) -> t++(' ':ts)
        _ -> t
    (_:d') -> extractString d'
    _ -> []

{-# INLINE extractText #-}
extractText :: DOM -> DOM
extractText d = case extractString d of
    (_:_) -> [(0,(MkElem (Text,extractString d,[])))]
    _ -> []

{-# INLINE parseStringToDOM #-}
parseStringToDOM :: ElemType -> Parser String -> DOM -> DOM
parseStringToDOM e f d = case d of
    ((_,(MkElem (Text,t,_))):d') -> (0,(MkElem (e,case parse f t of
        Consumed (Ok t' _) -> t'
        _ -> "VOID",[MkAttribute ("text",t)]))):parseStringToDOM e f d'
    _ -> []

---------------------------------------------------------------------------------

{-# INLINE moveRoot #-}
moveRoot :: Int -> DOM -> DOM
moveRoot i d = case d of
    ((j,e):d') -> (i+j,e):moveRoot i d'
    _ -> []

{-# INLINE domNull #-}
domNull :: ShowDOM
domNull dom = dom

{-# INLINE domText #-}
domText :: String -> ShowDOM
domText t = (\dom -> (0,MkElem (Text,t,[])):dom)

{-# INLINE domNobrText #-}
domNobrText :: String -> ShowDOM
domNobrText t = (\dom -> (0,MkElem (Text,formatNobr t,[])):dom) where

    formatNobr :: String -> String
    formatNobr [] = ""
    formatNobr (c:cs) = if isSpace c
        then '&':'n':'b':'s':'p':';':formatNobr cs
        else c:formatNobr cs

-- {-# INLINE domCode #-}
-- domCode :: IO () -> ShowDOM
-- domCode c = (\dom -> (0,MkCode c):dom)

{-# INLINE domHeader #-}
domHeader :: [Attribute] -> ShowDOM
domHeader [] = (\dom -> dom)
domHeader as = (\dom -> (0,MkElem (Header,"",as)):dom)

{-# INLINE domTag #-}
domTag :: String -> ShowDOM
domTag t = (\dom -> (0,MkElem (Tag,t,[])):dom)

{-# INLINE domAttrTag #-}
domAttrTag :: String -> [Attribute] -> ShowDOM
domAttrTag t as = (\dom -> (0,MkElem (Tag,t,as)):dom)

{-# INLINE domJoin #-}
domJoin :: DOM -> ShowDOM
domJoin [] = domNull
domJoin (d:ds) = (\dom -> d:(domJoin ds dom))

{-# INLINE domChild #-}
domChild :: ShowDOM -> ShowDOM
domChild d = (\dom -> domJoin (moveRoot 1 (d [])) dom)

{-# INLINE domMakePage #-}
domMakePage :: ShowDOM -> DOM
domMakePage d = (domTag "HTML" . domChild d) []

setAttribute :: Attribute -> [Attribute] -> [Attribute]
setAttribute a [] = [a]
setAttribute a@(MkAttribute (n,_)) (a0@(MkAttribute (n0,_)):as) = if (map toLower n)==(map toLower n0)
    then a:as
    else a0:setAttribute a as

setAttributes :: [Attribute] -> [Attribute] -> [Attribute]
setAttributes [] bs = bs
setAttributes (a:as) bs = setAttribute a (setAttributes as bs)

getLink :: String -> DOM -> Maybe String
getLink _ [] = Nothing
getLink t ((_,e):d) = case e of
    (MkElem (Tag,"A",as)) -> Just $ getAttributeAsString t as
    _ -> getLink t d

getLinkWithText :: String -> String -> DOM -> Maybe String
getLinkWithText _ _ [] = Nothing
getLinkWithText c t ((_,e):d) = case e of
    (MkElem (Tag,"A",as)) -> case d of
        ((_,MkElem (Text,x,_)):_) | (trimText x) == c -> Just $ getAttributeAsString t as
        _ -> getLinkWithText c t d
    _ -> getLinkWithText c t d
