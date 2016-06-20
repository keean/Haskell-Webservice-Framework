-- Lib/XML/Generator.hs (C)2001,2002 Keean Schupke
--
--  DOM -> XML Generator

module Lib.XML.Generator(showDOM,showElement,showSTag,showETag,showEmptyTag,showAttributes,
    showDocType,showComment,showText,ioHPutDOM) where

--import Data.Char
import System.IO
import Lib.XML.Types
import Lib.Monad.MonadIO

ioHPutDOM :: MonadIO m => Handle -> DOM -> m ()
ioHPutDOM h domIn = generateElement domIn where

    generateElement :: MonadIO m => [DomNode] -> m ()
    generateElement [] = return ()
    generateElement ((_,x):dom') = do
        case x of
            Flush -> ioHFlush h
            ETag n -> generateETag n
            STag _ _ -> generateSTag x
            EmptyTag _ _ -> generateEmptyTag x
            _ -> generateOther x 
        generateElement dom'
    
    generateEmptyTag :: MonadIO m => XmlElement -> m ()
    generateEmptyTag (EmptyTag n a) = do
        ioHPutChar h '<'
        ioHPutStr h n 
        generateAttributes a
        ioHPutStr h " />"
    generateEmptyTag (STag n a) = do
        ioHPutChar h '<' 
        ioHPutStr h n
        generateAttributes a
        ioHPutStr h " />"
    generateEmptyTag _ = return ()
    
    generateSTag :: MonadIO m => XmlElement -> m ()
    generateSTag (EmptyTag n a) = do
        ioHPutChar h '<'
        ioHPutStr h n
        generateAttributes a
        ioHPutStr h ">"
    generateSTag (STag n a) = do
        ioHPutChar h '<' 
        ioHPutStr h n
        generateAttributes a
        ioHPutStr h ">"
    generateSTag _ = return ()
    
    generateAttributes :: MonadIO m => [XmlAttribute] -> m ()
    generateAttributes [] = return ()
    generateAttributes (XmlAttribute (n,v):as) = do
        ioHPutChar h ' '
        ioHPutStr h n
        case v of 
            (_:_) -> do
                ioHPutStr h "=\""
                generateElements v
                ioHPutChar h '"'
                generateAttributes as
            _ -> generateAttributes as
    
    generateOther :: MonadIO m => XmlElement -> m ()
    generateOther (DocType tn sl pl) = do
        ioHPutStr h "<!DOCTYPE "
        ioHPutStr h tn
        case (pl,sl) of
            (p,s) | p/="" && s=="" -> do
                        ioHPutStr h " public \""
                        ioHPutStr h p 
                        ioHPutStr h "\">"
                    | p=="" && s/="" -> do
                        ioHPutStr h " system \""
                        ioHPutStr h s
                        ioHPutStr h "\">"
                    | p/="" && s/="" -> do
                        ioHPutStr h " public \""
                        ioHPutStr h p
                        ioHPutStr h "\" \""
                        ioHPutStr h s
                        ioHPutStr h "\">"
            _ -> ioHPutChar h '>'
    generateOther (Text e) = generateElements e
    generateOther (Comment s) = do
        ioHPutStr h "<!--" 
        ioHPutStr h s
        ioHPutStr h "-->"
    generateOther (PI n a) = do
        ioHPutStr h "<?"
        ioHPutStr h n
        ioHPutStr h a
        ioHPutStr h "?>"

    generateOther _ = return ()

    generateElements :: MonadIO m => [XmlElement] -> m ()
    generateElements [] = return ()
    generateElements (x0:xs) = do
        (case x0 of
            (CharData s) -> ioHPutStr h s
            (EntityRef e) -> do
                    ioHPutChar h '&'
                    ioHPutStr h e
                    ioHPutChar h ';'
            _ -> return ())
        generateElements xs
    
    generateETag :: MonadIO m => XmlTagName -> m ()
    generateETag n = do
        ioHPutStr h "</"
        ioHPutStr h n
        ioHPutChar h '>'

showDOM :: DOM -> ShowS
showDOM [] = id
showDOM ((_,x):ds) = showElement x . showDOM ds

showElement :: XmlElement -> ShowS
showElement (ETag n) = showETag n
showElement (STag n a) = showSTag n a
showElement (EmptyTag n a) = showEmptyTag n a
showElement (DocType n s p) = showDocType n s p
showElement (Text e) = showText e
showElement (Comment s) = showComment s
showElement _ = id
    
showEmptyTag :: XmlTagName -> [XmlAttribute] -> ShowS
showEmptyTag n a = showChar '<' . showString n . showAttributes a . showString " />"
    
showSTag :: XmlTagName -> [XmlAttribute] -> ShowS
showSTag n a = showChar '<' . showString n . showAttributes a . showChar '>'
    
showAttributes :: [XmlAttribute] -> ShowS
showAttributes [] = id 
showAttributes (XmlAttribute (n,v):as) = showChar ' ' . showString n . (case v of
    (_:_) -> showString "=\"" . showText v . showChar '"'
    _ -> id) . showAttributes as
    
showDocType :: XmlTagName -> XmlSystemLiteral -> XmlPubidLiteral -> ShowS
showDocType tn sl pl = showString "<!DOCTYPE " . showString tn . case (pl,sl) of
    (p,s) | p/="" && s=="" -> showString " public \"" . showString p . showString "\">"
            | p=="" && s/="" -> showString " system \"" . showString s . showString "\">"
            | p/="" && s/="" -> showString " public \"" . showString p . showString "\" \"" . showString s 
                . showString "\">"
    _ -> showChar '>'

showComment :: String -> ShowS
showComment s = showString "<!--" . showString s . showString "-->"

showText :: [XmlElement] -> ShowS
showText [] = id
showText (x0:xs) = (case x0 of
    (CharData s) -> showString s
    (EntityRef e) -> showChar '&' . showString e . showChar ';'
    _ -> id) .  showText xs
    
showETag :: XmlTagName -> ShowS
showETag n = showString "</" . showString n . showChar '>'

