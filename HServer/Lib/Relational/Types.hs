{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- hsql.hs 
-- Copyright (C) 2002,2004 Keean Schupke

module Lib.Relational.Types where

import Prelude
import Numeric
import Control.Monad.Error

import Lib.Monad.MonadState
import Lib.Monad.StateT
import Lib.DBC.Types
import Lib.TIR.HRecord

------------------------------------------------------------------------------
-- data types for SQL expressions

type Query a = (MonadPlus m, SqlIfIO m, SqlIO m, MonadState SqlState m) => StateT Int m a
type SqlTag = Int

------------------------------------------------------------------------------
-- relations 

data Relation typeRep = Relation typeRep SqlRelation deriving Show
data SqlTypeRep rows cols = SqlTypeRep rows cols deriving Show

data SingleRow = SingleRow deriving Show
data MultiRow = MultiRow deriving Show

data SqlRelation = SqlRelation {
	rTag :: SqlTag,							--	Unique relation ID
	rSource :: SqlSource,					-- the source of this relation 
	rRestrictList :: [SqlExpression],	-- row restrictions
	rProjectList :: [SqlExpression], 		-- column projections and extensions
	rGroupList :: [SqlExpression],
	rOrderList :: [SqlExpression],
	rLimit :: Maybe Int
}

sqlRelation :: SqlRelation
sqlRelation = SqlRelation {
	rTag = 0,
	rSource = SqlSourceNone,
	rRestrictList = [],
	rProjectList = [],
	rGroupList = [],
	rOrderList = [],
	rLimit = Nothing
}

instance Show SqlRelation where
	showsPrec _ r = showSqlRelation r

showSqlRelation :: SqlRelation -> ShowS
showSqlRelation r = showString "SELECT "
	. showProjectList (rTag r) (rProjectList r)
	. showString " FROM "
	. showSource (rSource r)
	. (if null (rRestrictList r)
		then id
		else showString " WHERE " . showRestrictList (rRestrictList r))
	. (case rLimit r of
		Just l -> showString " LIMIT " . showInt l
		Nothing -> id)
	. (if null (rOrderList r)
		then id
		else showString " ORDER BY " . showExpressionList (rOrderList r))
												{-nameExpressionList (rOrderList r) (rTag r))-}

showProjectList :: Int -> [SqlExpression] -> ShowS
showProjectList i (e0:es) = showSqlExpression e0
	. showString " AS "
	. nameSqlExpression e0 i
	. (if null es then id else showChar ',' . showProjectList i es)
showProjectList _ _ = id

showRestrictList :: [SqlExpression] -> ShowS
showRestrictList (e0:es) = showSqlExpression e0
	. if null es
		then id
		else showString " AND " . showRestrictList es
showRestrictList _ = id

showSource :: SqlSource -> ShowS
showSource (SqlSourceTable n) = showString n
showSource (SqlSourceRelation op j1 j2 on) = showChar '('
	. showSqlRelation j1 . showString ") AS "
	. nameSqlRelation j1 . showChar '_' . showInt (rTag j1)
	. showChar ' ' . showSqlRelationOp op
	. showString " (" . showSqlRelation j2 . showString ") AS "
	. nameSqlRelation j2 . showChar '_' . showInt (rTag j2)
	. showString " ON " . showRestrictList [on]
showSource (SqlSourceSet op s1 s2) = showString "(("
	. showSqlRelation s1 . showString ") " . showSqlSetOp op . showString " ("
	. showSqlRelation s2 . showString ")) AS " . nameSqlRelation s1
	. showChar '_' . nameSqlSetOp op . showChar '_' .  nameSqlRelation s2
showSource _ = id

nameSqlRelation :: SqlRelation -> ShowS
nameSqlRelation r = nameSource (rSource r)

nameSource :: SqlSource -> ShowS
nameSource (SqlSourceTable n) = showString n
nameSource (SqlSourceRelation op j1 j2 _) = nameSqlRelation j1 . showChar '_'
	. nameSqlRelationOp op . showChar '_' . nameSqlRelation j2
nameSource _ = id

------------------------------------------------------------------------------
-- attributes

data Attribute a b = Attribute AttrDetails deriving Show

data AttrDetails = AttrDetails {
	attrName :: SqlColumnName,
	attrSize :: Int,
	attrPrecision :: Int,
	attrScale :: Int,
	attrType :: String,
	attrTags :: [Int]
} deriving Show

attr :: AttrDetails
attr = AttrDetails {
	attrName = "",
	attrSize = 1,
	attrPrecision = 10,
	attrScale = 0,
	attrType = "",
	attrTags = []
}

data Attr = Attr {
	attrSqlType :: SqlType,
	attrDetails :: AttrDetails
}

showAttr :: Attr -> ShowS
showAttr a = (if attrType ad == ""
		then showSqlType (attrSqlType a)
		else showString (attrType ad))
	. (case attrSqlType a of
		SqlCharType -> showSize
		SqlVarcharType -> showSize
		_ -> id) where

	showSize :: ShowS
	showSize = showChar '(' . showInt (attrSize ad) . showChar ')'

	ad :: AttrDetails
	ad = attrDetails a

instance Show Attr where
	showsPrec _ a = showAttr a

------------------------------------------------------------------------------
-- tables 

data Table r = Table {
	sqlTableName :: SqlTableName,
	sqlTableType :: r
} deriving Show

newTable :: Record r => SqlTableName -> r -> Table r
newTable n r = Table {
	sqlTableName = n,
	sqlTableType = r
}

------------------------------------------------------------------------------

newtype SqlTyped a b = SqlTyped {
	sqlExpression :: SqlExpression
} deriving Show

data SqlSource = SqlSourceTable SqlTableName
	| SqlSourceSelect SqlRelation
	| SqlSourceRelation SqlRelationOp SqlRelation SqlRelation SqlExpression
	| SqlSourceSet SqlSetOp SqlRelation SqlRelation
	| SqlSourceNone
	deriving Show

data SqlExpression = SqlExpressionConst String
	| SqlExpressionColumn AttrDetails
	| SqlExpressionBinary SqlBinaryOp SqlExpression SqlExpression
	| SqlExpressionAggr SqlAggrOp SqlExpression
	| SqlExpressionRelation SqlRelation

nameSqlExpression :: SqlExpression -> Int ->  ShowS
nameSqlExpression (SqlExpressionColumn ad) i = showString (attrName ad)
	. showTags (attrTags ad) . showChar '_' . showInt i
nameSqlExpression (SqlExpressionConst _) i = showString "toSqlType" . showChar '_' . showInt i
nameSqlExpression (SqlExpressionBinary op l r) i = nameSqlExpression l i
	. showChar '_'
	. nameSqlBinaryOp op
	. showChar '_'
	. nameSqlExpression r i
nameSqlExpression _ _ = id

showTags :: [Int] -> ShowS
showTags (t0:ts) = showChar '_' . showInt t0 . showTags ts
showTags _ = id

showSqlExpression :: SqlExpression -> ShowS
showSqlExpression (SqlExpressionColumn ad) = showString (attrName ad) . showTags (attrTags ad)
showSqlExpression (SqlExpressionConst s) = showString s
showSqlExpression (SqlExpressionBinary op l r) = showChar '('
	. showSqlExpression l . showChar ' '
	. showSqlBinaryOp op . showChar ' '
	. showSqlExpression r
	. showChar ')' 
showSqlExpression (SqlExpressionRelation r) = showChar '('
	. showSqlRelation r
	. showChar ')'
showSqlExpression _ = id

instance Show SqlExpression where
	showsPrec _ e = showSqlExpression e

showSqlExpressionList :: [SqlExpression] -> ShowS
showSqlExpressionList e = showChar '(' . showExprList e . showChar ')'

showExprList :: [SqlExpression] -> ShowS
showExprList (e0:es) = showSqlExpression e0
	. if null es then id else showChar ',' . showExprList es
showExprList _ = id

showAssignments :: [SqlExpression] -> [SqlExpression] -> ShowS
showAssignments (e0:es) (x0:xs) = showSqlExpression e0 . showChar '='
	. showSqlExpression x0 . if (null es) || (null xs) then id
		else showChar ',' . showAssignments es xs
showAssignments _ _ = id

showExpressionList :: [SqlExpression] -> ShowS
showExpressionList (c0:cs) = showSqlExpression c0 . if (null cs) then id
	else showChar ',' . showExpressionList cs
showExpressionList _ = id

nameExpressionList :: [SqlExpression] -> Int -> ShowS
nameExpressionList (c0:cs) i = nameSqlExpression c0 i . if (null cs) then id
	else showChar ',' . nameExpressionList cs i
nameExpressionList _ _ = id

-----------------------------------------------------------------------------
-- Haskell -> SQL ToSqlTypeant Coersion

dbQuote :: String -> String
dbQuote (c0:cs) = case c0 of
	'\'' -> '\\':'\'':dbQuote cs
	c -> c:dbQuote cs
dbQuote _ = ""

class Show a => SqlShow a where
	sqlShow :: a -> ShowS
instance SqlShow String where
	sqlShow s = showChar '\'' . showString (dbQuote s) . showChar '\''
instance SqlShow Int where
	sqlShow i = showInt i
instance SqlShow Integer where
	sqlShow i = showInt i
instance SqlShow Float where
	sqlShow f = showFloat f
instance SqlShow Double where
	sqlShow f = showFloat f

-- user extendable
class ToSqlType t s where
	toSqlType :: s -> (SqlTyped s t)
instance ToSqlType SqlChar String where
	toSqlType s = SqlTyped (SqlExpressionConst $ sqlShow s "")
instance ToSqlType SqlVarchar String where
	toSqlType s = SqlTyped (SqlExpressionConst $ sqlShow s "")
instance ToSqlType SqlInteger Int where
	toSqlType i = SqlTyped (SqlExpressionConst $ sqlShow i "")
instance ToSqlType SqlSmallInt Int where
	toSqlType i = SqlTyped (SqlExpressionConst $ sqlShow i "")
instance ToSqlType SqlBigInt Integer where
	toSqlType i = SqlTyped (SqlExpressionConst $ sqlShow i "")
instance ToSqlType SqlFloat Float where
	toSqlType f = SqlTyped (SqlExpressionConst $ sqlShow f "")
instance ToSqlType SqlDouble Double where
	toSqlType f = SqlTyped (SqlExpressionConst $ sqlShow f "")
instance ToSqlType SqlNumeric Float where
	toSqlType f = SqlTyped (SqlExpressionConst $ sqlShow f "")
instance (SqlShow b,ToSqlType a b) => ToSqlType [a] [b] where
	toSqlType f = SqlTyped (SqlExpressionConst $ (showChar '(' . toSqlType' f) ")") where
		
toSqlType' :: SqlShow a => [a] -> ShowS
toSqlType' (f0:fs@(_:_)) = sqlShow f0 . showChar ',' . toSqlType' fs
toSqlType' (f0:_) = sqlShow f0
toSqlType' _ = id

class FromSqlType s t where
	fromSqlType :: SqlTyped t s -> String -> Maybe t
instance FromSqlType SqlInteger Int where
	fromSqlType _ s = case readSigned readDec s of
		((i,_):_) -> Just i
		_ -> Nothing
instance FromSqlType SqlNumeric Float where
	fromSqlType _ s = case readSigned readFloat s of
		((f,_):_) -> Just f
		_ -> Nothing
instance FromSqlType SqlChar String where
	fromSqlType _ s = Just s
instance FromSqlType SqlVarchar String where
	fromSqlType _ s = Just s
-- extend forall SQL types.

-----------------------------------------------------------------------------
-- Binary Ops

data SqlBinaryOp = SqlAdd | SqlSub | SqlMul | SqlDiv | SqlMod | SqlEq
	| SqlGe | SqlGt | SqlLt | SqlLe | SqlNe | SqlAnd | SqlOr | SqlIn

nameSqlBinaryOp :: SqlBinaryOp -> ShowS
nameSqlBinaryOp SqlAdd = showString "add"
nameSqlBinaryOp SqlSub = showString "sub"
nameSqlBinaryOp SqlMul = showString "mul"
nameSqlBinaryOp SqlDiv = showString "div"
nameSqlBinaryOp SqlMod = showString "mod"
nameSqlBinaryOp SqlEq = showString "eq"
nameSqlBinaryOp SqlNe = showString "ne"
nameSqlBinaryOp SqlLt = showString "lt"
nameSqlBinaryOp SqlGt = showString "gt"
nameSqlBinaryOp SqlLe = showString "le"
nameSqlBinaryOp SqlGe = showString "ge"
nameSqlBinaryOp SqlAnd = showString "and"
nameSqlBinaryOp SqlOr = showString "or"
nameSqlBinaryOp SqlIn = showString "in"

showSqlBinaryOp :: SqlBinaryOp -> ShowS
showSqlBinaryOp SqlAdd = showString "+"
showSqlBinaryOp SqlSub = showString "-"
showSqlBinaryOp SqlMul = showString "*"
showSqlBinaryOp SqlDiv = showString "/"
showSqlBinaryOp SqlMod = showString "%"
showSqlBinaryOp SqlEq = showString "="
showSqlBinaryOp SqlNe = showString "<>"
showSqlBinaryOp SqlLt = showString "<"
showSqlBinaryOp SqlGt = showString ">"
showSqlBinaryOp SqlLe = showString "<="
showSqlBinaryOp SqlGe = showString ">="
showSqlBinaryOp SqlAnd = showString "AND"
showSqlBinaryOp SqlOr = showString "OR"
showSqlBinaryOp SqlIn = showString "IN"


instance Show SqlBinaryOp where
	showsPrec _ b = showSqlBinaryOp b

sqlListOp :: SqlBinaryOp -> (SqlTyped a b -> SqlTyped [a] [b] -> SqlTyped Bool SqlChar)
sqlListOp o = \(SqlTyped x) (SqlTyped y) -> SqlTyped (SqlExpressionBinary o x y)

sqlBinaryOp :: SqlBinaryOp -> (SqlTyped a b -> SqlTyped a b -> SqlTyped a b)
sqlBinaryOp o = \(SqlTyped x) (SqlTyped y) -> SqlTyped (SqlExpressionBinary o x y)

sqlCmpOp :: SqlBinaryOp -> (SqlTyped a b -> SqlTyped a b -> SqlTyped Bool SqlChar)
sqlCmpOp o = \(SqlTyped x) (SqlTyped y) -> SqlTyped (SqlExpressionBinary o x y)

infix 2 `elem` 
elem :: SqlTyped a b -> SqlTyped [a] [b] -> SqlTyped Bool SqlChar
elem = sqlListOp SqlIn 

infix 2 `and`
and :: SqlTyped Bool SqlChar -> SqlTyped Bool SqlChar -> SqlTyped Bool SqlChar 
and = sqlBinaryOp SqlAnd

infix 2 `or`
or :: SqlTyped Bool SqlChar -> SqlTyped Bool SqlChar -> SqlTyped Bool SqlChar
or = sqlBinaryOp SqlOr

infix 3 `lt`
lt :: SqlTyped a b -> SqlTyped a b -> SqlTyped Bool SqlChar
lt = sqlCmpOp SqlLt

infix 3 `gt`
gt :: SqlTyped a b -> SqlTyped a b -> SqlTyped Bool SqlChar
gt = sqlCmpOp SqlGt

infix 3 `le`
le :: SqlTyped a b -> SqlTyped a b -> SqlTyped Bool SqlChar
le = sqlCmpOp SqlLe

infix 3 `ge`
ge :: SqlTyped a b -> SqlTyped a b -> SqlTyped Bool SqlChar
ge = sqlCmpOp SqlGe

{-
eq :: SqlTyped a b -> SqlTyped a b -> SqlTyped Bool SqlChar
eq = sqlCmpOp SqlEq
-}

infix 3 `eq`
class EqSql x y where
	eq :: x -> y -> SqlTyped Bool SqlChar
instance ToSqlType t d => EqSql (SqlTyped d t) d where
	eq s h = sqlCmpOp SqlEq s (toSqlType h)
instance ToSqlType t d => EqSql d (SqlTyped d t) where
	eq h = sqlCmpOp SqlEq (toSqlType h)
instance EqSql (SqlTyped d t) (SqlTyped d t) where
	eq = sqlCmpOp SqlEq

infix 3 `ne`
ne :: SqlTyped a b -> SqlTyped a b -> SqlTyped Bool SqlChar
ne = sqlCmpOp SqlNe

infix 4 `add`
add :: SqlTyped a b -> SqlTyped a b -> SqlTyped a b
add = sqlBinaryOp SqlAdd

infix 4 `sub`
sub :: SqlTyped a b -> SqlTyped a b -> SqlTyped a b
sub = sqlBinaryOp SqlSub

infix 5 `mul`
mul :: SqlTyped a b -> SqlTyped a b -> SqlTyped a b
mul = sqlBinaryOp SqlMul

infix 5 `div`
div :: SqlTyped a b -> SqlTyped a b -> SqlTyped a b
div = sqlBinaryOp SqlDiv

infix 5 `mod`
mod :: SqlTyped a b -> SqlTyped a b -> SqlTyped a b
mod = sqlBinaryOp SqlMod

-----------------------------------------------------------------------------
-- Aggregate Ops

data SqlAggrOp = SqlCount | SqlSum | SqlMax | SqlMin | SqlAvg | SqlStdDev | SqlVariance

showSqlAggrOp :: SqlAggrOp -> ShowS
showSqlAggrOp SqlCount = showString "COUNT"
showSqlAggrOp SqlSum = showString "SUM"
showSqlAggrOp SqlMax = showString "MAX"
showSqlAggrOp SqlMin = showString "MIN"
showSqlAggrOp SqlAvg = showString "AVG"
showSqlAggrOp SqlStdDev = showString "STDDEV"
showSqlAggrOp SqlVariance = showString "VARIANCE"

instance Show SqlAggrOp where
	showsPrec _ a = showSqlAggrOp a

-----------------------------------------------------------------------------
-- Relational Ops

data SqlRelationOp = SqlInnerJoin | SqlLeftJoin | SqlRightJoin | SqlFullJoin 
	{- | SqlSemiJoin | SqlAntiSemiJoin -}

nameSqlRelationOp :: SqlRelationOp -> ShowS
nameSqlRelationOp SqlInnerJoin = showString "innerjoin"
nameSqlRelationOp SqlLeftJoin = showString "leftjoin"
nameSqlRelationOp SqlRightJoin = showString "rightjoin"
nameSqlRelationOp SqlFullJoin = showString "fulljoin"
{-
nameSqlRelationOp SqlSemiJoin = showString "semijoin"
nameSqlRelationOp SqlAntiSemiJoin = showString "antisemijoin"
-}

showSqlRelationOp :: SqlRelationOp -> ShowS
showSqlRelationOp SqlInnerJoin = showString "JOIN"
showSqlRelationOp SqlLeftJoin = showString "LEFT JOIN"
showSqlRelationOp SqlRightJoin = showString "RIGHT JOIN"
showSqlRelationOp SqlFullJoin = showString "FULL JOIN"
{-
showSqlRelationOp SqlSemiJoin = showString "WHERE EXISTS"
showSqlRelationOp SqlAntiSemiJoin = showString "WHERE NOT EXISTS"
-}

instance Show SqlRelationOp where
	showsPrec _ r = showSqlRelationOp r

-----------------------------------------------------------------------------
-- Set Ops

data SqlSetOp = SqlUnion | SqlIntersection | SqlDifference

nameSqlSetOp :: SqlSetOp -> ShowS
nameSqlSetOp SqlUnion = showString "union"
nameSqlSetOp SqlIntersection = showString "intsersect"
nameSqlSetOp SqlDifference = showString "difference"

showSqlSetOp :: SqlSetOp -> ShowS
showSqlSetOp SqlUnion = showString "UNION"
showSqlSetOp SqlIntersection = showString "INTERSECT"
showSqlSetOp SqlDifference = showString "EXCEPT"

instance Show SqlSetOp where
	showsPrec _ s = showSqlSetOp s

