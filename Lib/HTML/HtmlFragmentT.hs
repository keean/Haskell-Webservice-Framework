{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Haskell HTML module
-- Copyright (C) 2002 Keean Schupke

module Lib.HTML.HtmlFragmentT(HtmlFragmentT(..),HtmlTableT(..),
    HtmlRowT(..)) where
    
--import Data.Char
import Control.Monad

import Lib.Monad.MonadT
import Lib.XML.Types
import Lib.XML.MonadDom
import Lib.XML.DomT
--import Lib.HTML.Types
import Lib.HTML.MonadHtml
import GHC.Base

------------------------------------------------------------------------------

newtype HtmlFragmentT m a = HFT {hft :: DomT m a}
newtype HtmlTableT m a = HTT {htt :: DomT m a}
newtype HtmlRowT m a = HRT {hrt :: DomT m a}

instance (Functor m, Monad m) => Functor (HtmlFragmentT m) where
    fmap f (HFT a) = HFT $ fmap f a

instance (Applicative m, Monad m) => Applicative (HtmlFragmentT m) where
    pure x = HFT $ pure x
    (HFT m) <*> (HFT n) = HFT $ m <*> n

instance Monad m => Monad (HtmlFragmentT m) where
    return a = HFT $ return a
    (HFT m) >>= n = HFT $ m >>= \a -> hft $ n a

instance (Monad m, MonadPlus m) => Alternative (HtmlFragmentT m) where
    empty = HFT $ mzero
    (HFT fm) <|> (HFT xm) = HFT $ fm <|> xm

instance MonadPlus m => MonadPlus (HtmlFragmentT m) where
    mzero = HFT $ mzero
    (HFT a) `mplus` (HFT b) = HFT $ a `mplus` b

instance (Functor m, Monad m) => Functor (HtmlTableT m) where
    fmap f (HTT n) = HTT $ fmap f n

instance (Applicative m, Monad m) => Applicative (HtmlTableT m) where
    pure x = HTT $ pure x
    (HTT m) <*> (HTT n) = HTT $ m <*> n

instance Monad m => Monad (HtmlTableT m) where
    return a = HTT $ return a
    (HTT m) >>= n = HTT $ m >>= \a -> htt $ n a

instance (Functor m, Monad m) => Functor (HtmlRowT m) where
    fmap f (HRT a) = HRT $ fmap f a

instance (Applicative m, Monad m) => Applicative (HtmlRowT m) where
    pure x = HRT $ pure x
    (HRT m) <*> (HRT n) = HRT $ m <*> n

instance Monad m => Monad (HtmlRowT m) where
    return a = HRT $ return a
    (HRT m) >>= n = HRT $ m >>= \a -> hrt $ n a

instance Monad m => MonadT HtmlFragmentT m where
    up = HFT . up 
    down = down . hft

instance Monad m => MonadT HtmlTableT m where
    up = HTT . up
    down = down . htt 

instance Monad m => MonadT HtmlRowT m where
    up = HRT . up
    down = down . hrt

instance Monad m => MonadHtmlBase (HtmlFragmentT m)
instance Monad m => MonadHtmlBase (HtmlTableT m)
instance Monad m => MonadHtmlBase (HtmlRowT m)

instance Monad m => MonadHtml (HtmlFragmentT m) where

    htmlElement e = HFT $ domElem e
    htmlContainer e@(STag s _) (HFT f) = HFT $ do
        domElem e
        domBegin s
        a <- f
        domEnd
        return a
    htmlContainer _ f = f

    htmlText [] = return ()
    htmlText e = HFT $ domElem $ Text $ formatText e
    htmlNobrText s = HFT $ domElem $ Text $ formatNobr s []
    htmlNobr (HFT f) = HFT $ domContainer "NOBR" [] f
    attrBR a = HFT $ domElem $ EmptyTag "BR" (toXml a)
    htmlBR = HFT $ domElem $ EmptyTag "BR" [] 

    htmlDoc (HFT f) = HFT $ domContainer "HTML" [] f
    htmlHead (HFT f) = HFT $ domContainer "HEAD" [] f
    htmlTitle s = HFT $ domContainer "TITLE" [] $ domElem (Text [CharData s])
    htmlBody (HFT f) = HFT $ domContainer "BODY" [] f
    attrBody a (HFT f) = HFT $ domContainer "BODY" a f
    htmlBlockquote (HFT f) = HFT $ domContainer "BLOCKQUOTE" [] f
    htmlSmall (HFT f) = HFT $ domContainer "SMALL" [] f
    htmlBig (HFT f) = HFT $ domContainer "BIG" [] f
    htmlB (HFT f) = HFT $ domContainer "B" [] f
    htmlH1 (HFT f) = HFT $ domContainer "H1" [] f
    htmlH2 (HFT f) = HFT $ domContainer "H2" [] f
    htmlH3 (HFT f) = HFT $ domContainer "H3" [] f
    htmlH4 (HFT f) = HFT $ domContainer "H4" [] f
    htmlForm method action (HFT f) = HFT $ domContainer "FORM" [MkAttribute ("action",action),
        MkAttribute ("method",method)] f
    htmlSub (HFT f) = HFT $ domContainer "SUB" [] f
    htmlSuper (HFT f) = HFT $ domContainer "SUP" [] f
    htmlP (HFT f) = HFT $ domContainer "P" [] f
    htmlCenter (HFT f) = HFT $ domContainer "Center" [] f
    htmlCheckbox s b = HFT $ domElem $ EmptyTag "INPUT" $
        (\r -> if b then XmlAttribute ("checked",[]):r else r) $
        toXml [MkAttribute ("type","checkbox"),MkAttribute ("name",s)]
    
    htmlSubmit s = HFT $ domElem $ EmptyTag "INPUT" (toXml $ submitAttr s) 
    attrSubmit a s = HFT $ domElem $ EmptyTag "INPUT" (toXml (setAttributes a $ submitAttr (formatString s))) 

    htmlPop = HFT $ domPop
    htmlPush d = HFT $ domPush d
    attrFont a (HFT f) = HFT $ domContainer "FONT" a f
    attrP a (HFT f) = HFT $ domContainer "P" a f
    htmlSpan (HFT f) = HFT $ domContainer "SPAN" [] f
    htmlDiv (HFT f) = HFT $ domContainer "DIV" [] f
    attrSpan a (HFT f) = HFT $ domContainer "SPAN" a f
    attrDiv a (HFT f) = HFT $ domContainer "DIV" a f

    htmlHidden s d = HFT $ domElem $ EmptyTag "INPUT" (toXml $ hiddenAttr (formatString s) d) 
    attrPasswordEdit a s = HFT $ domElem $ EmptyTag "INPUT" (toXml (setAttributes a $ passwordEditAttr (formatString s))) 
    htmlTextEdit s d = HFT $ domElem $ EmptyTag "INPUT" (toXml $ textEditAttr (formatString s) d) 
    attrTextEdit a s d = HFT $ domElem $ EmptyTag "INPUT" (toXml (setAttributes a $ textEditAttr (formatString s) d)) 

    htmlLink s (HFT f) = HFT $ domContainer "A" (linkAttr s) f
    attrLink s a (HFT f) = HFT $ domContainer "A" (setAttributes a $ linkAttr s) f

    htmlImg s = HFT $ domElem $ STag "IMG" (toXml $ imgAttr s) 
    attrImg a s = HFT $ domElem $ STag "IMG" (toXml (setAttributes a $ imgAttr s)) 

    htmlNullImg = HFT $ domElem $ STag "IMG" (toXml nullImgAttr) 
    attrNullImg a = HFT $ domElem $ STag "IMG" (toXml (setAttributes a nullImgAttr)) 

instance Monad m => MonadHtmlToRow (HtmlFragmentT m) (HtmlRowT m) where
    htmlTD (HFT f) = HRT $ domContainer "TD" tdAttr f
    attrTD a (HFT f) = HRT $ domContainer "TD" (setAttributes a tdAttr) f
    htmlFragmentToColumn (HFT f) = HRT f

instance Monad m => MonadHtmlToTable (HtmlFragmentT m) (HtmlTableT m) where
    htmlFragmentToRow (HFT f) = HTT f

instance Monad m => MonadRowToTable (HtmlRowT m) (HtmlTableT m) where
    htmlTR (HRT f) = HTT $ domContainer "TR" [] f

instance Monad m => MonadTableToHtml (HtmlTableT m) (HtmlFragmentT m) where
    htmlTable (HTT f) = HFT $ domContainer "TABLE" tableAttr f
    attrTable a (HTT f) = HFT $ domContainer "TABLE" (setAttributes a tableAttr) f

