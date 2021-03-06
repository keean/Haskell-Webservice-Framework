{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

-- Haskell SQL module
-- Copyright (C) 2002 Keean Schupke

module Lib.DBC.Query (doQuery,(<?>)) where

import IO
import Data.FiniteMap

import Lib.DBC.Types
import Lib.DBC.HIODBC
import Lib.DBC.HSQL

infix 8 <?>
(<?>) :: ShowConst a => SqlColMap -> TypedExpr a -> String
(<?>) cmap (TypedExpr (SqlAliasExpr (SqlLabel s))) = lookupWithDefaultFM cmap "(NULL)" s
(<?>) _ _ = "(NULL)"

doQuery :: Query a -> SqlHandle (ColNameList,a)
doQuery qry = case query qry (QStateNil (LTag 0)) of
	Ok r q -> do
		clist <- sqlQuery (show q)
		return (clist,r)
	_ -> fail "Query compilation error."

