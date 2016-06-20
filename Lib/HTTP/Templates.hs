-- Copyright (C) 2002 Keean Schupke

module Lib.HTTP.Templates (stringType) where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

------------------------------------------------------------------------------
-- stringType

stringType :: String -> Q [Dec]
stringType s = do
    x <- qNewName "x"
    y <- qNewName "y"
    n <- newtypeD (cxt []) (mkName s) [] (normalC (mkName s)
        [(strictType notStrict (conT $ mkName "String"))]) []
    t <- sigD (mkName $ "show"++s) (appT (appT arrowT (conT $ mkName s)) (conT $ mkName "ShowS"))
    f <- funD (mkName $ "show"++s) [clause [conP (mkName s) [varP x]]
        (normalB (appE (varE $ mkName "showString") (varE x))) []]
    i0 <- instanceD (cxt []) (appT (conT $ mkName "Show") (conT $ mkName s)) [
        funD (mkName "showsPrec") [clause [wildP,varP x]
        (normalB (appE (varE (mkName $ "show"++s)) (varE x))) []]]
    i1 <- instanceD (cxt []) (appT (conT $ mkName "Eq") (conT $ mkName s)) [
        funD (mkName "==") [clause [conP (mkName s) [varP x],conP (mkName s) [varP y]]
        (normalB (appE (appE (varE $ mkName "==") (varE x)) (varE y))) []]]
    return [n,t,f,i0,i1]

