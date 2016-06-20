-- Haskell SQL module
-- Copyright (C) 2002 Keean Schupke

module Lib.HTML.Types (HtmlFragment(..),HtmlTable(..),HtmlRow(..),
	HtmlFragmentT(..),HtmlTableT(..),HtmlRowT(..)) where
	
import Lib.XML.Dom
import Lib.XML.DomT

-- type HtmlForm = URI -> UriParameters -> HtmlFragment ()

newtype HtmlFragment a = HF {hf :: Dom a}
newtype HtmlTable a = HT {ht :: Dom a}
newtype HtmlRow a = HR {hr :: Dom a}

newtype HtmlFragmentT m a = HFT {hft :: DomT m a}
newtype HtmlTableT m a = HTT {htt :: DomT m a}
newtype HtmlRowT m a = HRT {hrt :: DomT m a}

