{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-} 

-- parser.hs: Copyright (C)2001,2002 Keean Schupke.
--
--		Polymorphic monadic consumer based parser.

module Lib.XML.Dom(Dom(..)) where

import Control.Monad hiding (guard)
import Lib.Arrow.Runnable
import Lib.XML.Types
import Lib.XML.MonadDom

------------------------------------------------------------------------------

newtype Dom a = DOM { dom :: (XmlTreeDepth,[XmlTagName],ShowDOM) -> (a,(XmlTreeDepth,[XmlTagName],ShowDOM)) }

instance Monad Dom where
	return a = DOM $ \d -> (a,d)
	(DOM m) >>= n = DOM $ \d -> (\(a',d') -> dom (n a') d') (m d)

instance Runnable (Dom a) (ShowDOM -> (a,ShowDOM)) where
	run m = \d -> case (dom m) (0,[],d) of (a,(_,_,d')) -> (a,d')

instance MonadDom Dom where
	domBegin s = DOM $ \(i,t,d) -> ((),(i+1,s:t,d))
	domEnd = DOM $ \(i,t@(t0:ts),d) -> if i>0 then ((),(i-1,ts,d . \f -> (i-1,ETag t0):f)) else ((),(i,t,d))
	domElem e = DOM $ \(i,t,d) -> ((),(i,t,d . \f -> (i,e):f ))
	domPop = DOM $ \(i,t,d) -> (d,(i,t,id))
	(DOM m) `domBreak` n = DOM $ \d -> (\(a',d') -> (n a',d')) (m d)
	domPush dm = DOM $ \(i,t,d) -> ((),(i,t,d . prepend i dm))
	domContainer s attr frag = do
		domElem $ STag s (toXml attr)
		domBegin s
		a <- frag
		domEnd
		return a

