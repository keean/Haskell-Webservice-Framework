{-# LANGUAGE FlexibleContexts #-}

-- Lib/HTML/DOM.hs (C)2002 Keean Schupke
--
-- DOM Types.

module Lib.HTML.DOM(htmlToDOM,scriptParser) where

import Data.Char
import Control.Monad

import Lib.XML.Types
import Lib.XML.Parser
import Lib.Parser.Parser
import Lib.Monad.MonadParser

htmlToDOM :: String -> DOM
htmlToDOM s = xmlParser htmlRules s

htmlRules :: XmlRules
htmlRules = XmlRules {
    isTerminal = \t -> case t of
        "BR" -> True
        "HR" -> True
        "WBR" -> True
        "IMG" -> True
        "INPUT" -> True
        "META" -> True
        "LINK" -> True
        "AREA" -> True
        "PARAM" -> True
        "FRAME" -> True
        "COL" -> True
        "SPACER" -> True
        _ -> False,
    isCloseOnExit = \t -> case t of
        ("HTML",_) -> True
        ("HEAD",a) | a/="HTML" -> True
        ("BODY",a) | a/="HTML" -> True
        ("TABLE","TR") -> True
        ("TABLE","TD") -> True
        ("TABLE","TH") -> True
        ("TR","TD") -> True
        ("SELECT","OPTION") -> True
        ("BLOCKQUOTE","P") -> True
        ("DIV",a) | a/="HTML" && a/="BODY"-> True
        ("LAYER",a) | a/="HTML" && a/="BODY"-> True
        ("TD",a) | a/="TR" && a/="TABLE" && a/="BODY" && a/="HTML" -> True
        ("FRAMESET","FRAME") -> True
        _ -> False,
    isCloseOnEntry = \t -> case t of
        ("HTML",_) -> True
        ("TR","TR") -> True
        ("TD","TD") -> True
        (a,"COLGROUP") | a/="COL" -> True
        ("BODY",a) | a/="HTML" -> True
        ("OPTION","OPTION") -> True
        ("LI","LI") -> True
        ("P","P") -> True
        ("BLOCKQUOTE","P") -> True
        ("TABLE","A") -> True
        ("TR","A") -> True
        ("TD","A") -> True
        _ -> False,
    specialParser = \t -> case t of
        "SCRIPT" -> scriptParser
        "STYLE" -> styleParser
        _ -> mzero
}
    
scriptParser :: Parser String
scriptParser = untilP (literal (fmap toUpper item) "</SCRIPT>") item

styleParser :: Parser String
styleParser = untilP (literal (fmap toUpper item) "</STYLE>") item
    
