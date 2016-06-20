
-- Haskell HTML module
-- Copyright (C) 2002 Keean Schupke

module Lib.XML.MonadDom(MonadDom(..),setAttributes,fmToAttributes,toXml,prepend) where
    
--import Control.Monad
import qualified Data.Map as Map

import Lib.XML.Types

------------------------------------------------------------------------------

class Monad f => MonadDom f where
    domBegin :: String -> f ()
    domEnd :: f ()
    domElem :: XmlElement -> f ()
    domPop :: f ShowDOM
    domBreak :: f a -> (a -> f b) -> f (f b)
    domPush :: [DomNode] -> f ()
    domContainer :: String -> [Attribute] -> f a -> f a
    
setAttributes :: [Attribute] -> [Attribute] -> [Attribute]
setAttributes [] bs = bs
setAttributes (a0:as) bs = setAttribute a0 (setAttributes as bs)

setAttribute :: Attribute -> [Attribute] -> [Attribute]
setAttribute a [] = [a]
setAttribute a@(MkAttribute (n,_)) (a0@(MkAttribute (n0,_)):as) = if n==n0
   then a:as
       else a0:setAttribute a as

fmToAttributes :: Map.Map String String -> [Attribute]
fmToAttributes fm = map (\a -> MkAttribute a) (Map.toList fm)

toXml :: [Attribute] -> [XmlAttribute]
toXml [] = []
toXml (MkAttribute (n,v):as) = XmlAttribute (n,[CharData v]) : toXml as

prepend :: Int -> [DomNode] -> ShowDOM
prepend _ [] d = d
prepend i ((i0,e0):ps) d = (i+i0,e0):prepend i ps d

