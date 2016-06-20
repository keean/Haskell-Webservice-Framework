-- Haskell SQL module
-- Copyright (C) 2002 Keean Schupke

module Lib.Server.Error (errorReport) where

import Lib.XML.Types
import Lib.HTML.MonadHtml
import Lib.HTML.HtmlFragment

serverVersion :: Double
serverVersion = 0.1

errorReport :: String -> HtmlFragment ()
errorReport err = htmlBody $ do
    htmlH1 $ htmlNobrText $ (showString "&lt;HyperServer&nbsp;" . shows serverVersion . showString "&gt;") ""
    htmlBlockquote $ do
        htmlH2 $ htmlText [CharData "Copyright ",EntityRef "copy",CharData " 2002 Keean Schupke"]
        htmlH3 $ htmlNobrText $ showString err ""

