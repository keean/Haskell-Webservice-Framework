{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-- Haskell HTML module
-- Copyright (C) 2002 Keean Schupke

module Lib.HTML.HtmlFragment(HtmlFragment(..),HtmlTable(..),
	HtmlRow(..)) where
	
import Char
import Monad

import Lib.Monad.MonadT
import Lib.Arrow.Runnable
import Lib.XML.Types
import Lib.XML.MonadDom
import Lib.XML.Dom
import Lib.HTML.Types
import Lib.HTML.MonadHtml

------------------------------------------------------------------------------

instance Monad HtmlFragment where
	return a = HF $ return a
	(HF m) >>= n = HF $ m >>= \a -> hf $ n a

instance Monad HtmlTable where
	return a = HT $ return a
	(HT m) >>= n = HT $ m >>= \a -> ht $ n a

instance Monad HtmlRow where
	return a = HR $ return a
	(HR m) >>= n = HR $ m >>= \a -> hr $ n a

instance Runnable (HtmlFragment a) (Dom a) where
	run m = hf m

instance Runnable (HtmlTable a) (Dom a) where
	run m = ht m

instance Runnable (HtmlRow a) (Dom a) where
	run m = hr m

instance MonadHtmlBase HtmlFragment
instance MonadHtmlBase HtmlTable
instance MonadHtmlBase HtmlRow

instance MonadHtml HtmlFragment where

	htmlElement e = HF $ domElem e
	htmlContainer e@(STag s _) (HF f) = HF $ do
		domElem e
		domBegin s
		a <- f
		domEnd
		return a
	htmlContainer _ f = f

	htmlText [] = return ()
	htmlText e = HF $ domElem $ Text $ formatText e
 	htmlNobrText e = HF $ domElem $ Text $ formatNobr e []
	htmlBR = HF $ domElem $ EmptyTag "BR" []
	attrBR a = HF $ domElem $ EmptyTag "BR" (toXml a)

	htmlDoc (HF f) = HF $ domContainer "HTML" [] f
	htmlHead (HF f) = HF $ domContainer "HEAD" [] f
	htmlTitle s = HF $ domContainer "TITLE" [] $ domElem (Text [CharData s])
	htmlBody (HF f) = HF $ domContainer "BODY" [] f
	attrBody a (HF f) = HF $ domContainer "BODY" a f
	htmlBlockquote (HF f) = HF $ domContainer "BLOCKQUOTE" [] f
	htmlSmall (HF f) = HF $ domContainer "SMALL" [] f
	htmlBig (HF f) = HF $ domContainer "BIG" [] f
	htmlB (HF f) = HF $ domContainer "B" [] f
	htmlH1 (HF f) = HF $ domContainer "H1" [] f
	htmlH2 (HF f) = HF $ domContainer "H2" [] f
	htmlH3 (HF f) = HF $ domContainer "H3" [] f
	htmlH4 (HF f) = HF $ domContainer "H4" [] f
	htmlForm method action (HF f) = HF $ domContainer "FORM" [MkAttribute ("method",method),
		MkAttribute ("action",action)] f
	htmlSub (HF f) = HF $ domContainer "SUB" [] f
	htmlSuper (HF f) = HF $ domContainer "SUP" [] f
	htmlP (HF f) = HF $ domContainer "P" [] f
	htmlCenter (HF f) = HF $ domContainer "CENTER" [] f
	htmlCheckbox s b = HF $ domElem $ EmptyTag "INPUT" $ 
		(\r -> if b then XmlAttribute ("checked",[]):r else r) $
		toXml [MkAttribute ("type","checkbox"),MkAttribute ("name",s)]
	htmlSubmit s = HF $ domElem $ EmptyTag "INPUT" (toXml [MkAttribute ("type","submit"),MkAttribute ("value",s)]) 
	htmlPop = HF $ domPop

	attrFont a (HF f) = HF $ domContainer "FONT" a f
	attrP a (HF f) = HF $ domContainer "P" a f



instance MonadHtmlToRow HtmlFragment HtmlRow where
	htmlTD (HF f) = HR $ domContainer "TD" tdAttr f
	attrTD a (HF f) = HR $ domContainer "TD" (setAttributes a tdAttr) f

instance MonadRowToTable HtmlRow HtmlTable where
	htmlTR (HR f) = HT $ domContainer "TR" [] f

instance MonadTableToHtml HtmlTable HtmlFragment where
	htmlTable (HT f) = HF $ domContainer "TABLE" tableAttr f
	attrTable a (HT f) = HF $ domContainer "TABLE" (setAttributes a tableAttr) f

