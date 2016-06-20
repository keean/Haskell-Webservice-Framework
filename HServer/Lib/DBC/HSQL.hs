{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- hsql.hs 
-- Copyright (C) 2002 Keean Schupke

module Lib.DBC.HSQL (SqlTable(..),verifyTables,Query,Select,
	lt,gt,le,ge,eq,ne,add,sub,mul,div,mod,and,or,union,intersection,difference,
	query,sqlTable,table,relation,select,join,cross,leftjoin,rightjoin,semijoin,antisemijoin,
	Join(..),from,attribute,SqlExpression(..),SqlTyped(..),QState(..),newQState,Project(..),
	SqlReference(..),table',project',Restrict(..),ProjectFrom(..),Const(..),SelectState(..)) where

import Prelude hiding (or,and,mod,div)
import Numeric
import Char
import qualified Monad

-- import Lib.Arrow.Runnable
import Lib.Monad.MonadIO
import Lib.Monad.MonadState
import Lib.Monad.MonadT
import Lib.Monad.StateT
import Lib.DBC.Types
import Lib.DBC.HIODBC
import qualified Lib.TIR.TIR as TIR
import Data.FiniteMap
import Data.Dynamic
import Data.Generics
import Data.Set (Set,mkSet,elementOf)

------------------------------------------------------------------------------
-- Check DB tables against Haskell Schema

verifyTables :: Data a => [SqlTable a] -> SqlHandle ()
verifyTables (t0:ts) = do
	showTables "" "" "" ""
	tableInfoNames <- getColNameList
	tableNameList <- getTableNames tableInfoNames
	closeCursor
	verifyTable t0 (mkSet tableNameList)
	verifyTables ts
	return ()
verifyTables _ = return ()

getTableNames :: ColNameList -> SqlHandle [String]
getTableNames clist = do
	status <- fetch
	case status of
		SqlSuccess -> do
			row <- getRowAsFM clist
			tableName <- return (lookupWithDefaultFM row "" "TABLE_NAME")
			rows <- getTableNames clist
			return (tableName:rows)
		_ -> return []

verifyTable :: Data a => SqlTable a -> Set SqlTableName ->  SqlHandle ()
verifyTable table tableSet = case (sqlTableName table) of
	tn	| tn `elementOf` tableSet -> do
			showColumns "" "" tn ""
			clist <- getColNameList
			colMap <- getColTypeFM clist
			closeCursor
			colTypes <- toColTypes tableTypes
			vt colMap tn colTypes
		| otherwise -> do
			colTypes <- toColTypes tableTypes
			createTable tn colTypes
	where

	tableTypes :: [(SqlExpression,SqlType)]
	tableTypes = (getColumnTypes . sqlTableType) table

	toColTypes :: MonadIO m => [(SqlExpression,SqlType)] -> m [(SqlColumnName,SqlType)]
	toColTypes ((e,t):ts) = case e of
		SqlExpressionColumnName n -> do
			cs <- toColTypes ts
			return ((n,t):cs)
		_ -> ioUserException ((showString "Table "
			. showString (sqlTableName table)) " has bad attribute definition.")
	toColTypes _ = return []


createTable :: SqlTableName -> [(SqlColumnName,SqlType)] -> SqlHandle ()
createTable tableName cs = do
	sqlDo ((showString "CREATE TABLE "
		. showString tableName
		. showString "( "
		. createColumns cs) " )")

createColumns :: [(SqlColumnName,SqlType)] -> ShowS
createColumns ((cn,ct):cs@(_:_)) = showString cn
	. showChar ' '
	. showSqlType ct
	. showString ", "
	. createColumns cs
createColumns [(cn,ct)] = showString cn
	. showChar ' '
	. showSqlType ct
createColumns _ = id

vt :: FiniteMap SqlColumnName String -> SqlTableName -> [(SqlColumnName,SqlType)] -> SqlHandle ()
vt cm tn ((cn,ct):cs) = do
 case lookupFM cm (map toLower cn) of
	Just t
		| tp == ct -> vt cm tn cs
		| otherwise -> ioUserException ((showString "Table "
			. showString tn
			. showString " column "
			. showString cn
			. showString " has type "
			. showSqlType tp
			. showString " in database: expected type "
			. showSqlType ct) ".")
		where
			tp = readSqlType t
	Nothing -> do
		sqlDo ((showString "ALTER TABLE "
			. showString tn
			. showString " ADD COLUMN "
			. showString cn
			. showChar ' '
			. showSqlType ct) "")
		vt cm tn cs
vt _ _ [] = return ()

readSqlType :: String -> SqlType
readSqlType s = case readDec s of
	((i,_):_) -> toEnum i
	_ -> toEnum 0

getColTypeFM :: ColNameList -> SqlHandle (FiniteMap SqlColumnName String)
getColTypeFM clist = do
	status <- fetch
	case status of
		SqlSuccess -> do
			row <- getRowAsFM clist
			colName <- return $ map toLower (lookupWithDefaultFM row "" "COLUMN_NAME")
			colType <- return (lookupWithDefaultFM row "" "DATA_TYPE")
			rows <- getColTypeFM clist
			return (addToFM rows colName colType)
		_ -> return emptyFM

------------------------------------------------------------------------------

getColumnTypes :: Data t => t -> [(SqlExpression,SqlType)]
getColumnTypes a = everything (++) ([] `mkQ` getSmallIntColumns
	`extQ` getIntegerColumns
	`extQ` getBigIntColumns
	`extQ` getFloatColumns
	`extQ` getDoubleColumns
	`extQ` getCharColumns) a

getSmallIntColumns :: SqlTyped SqlSmallInt -> [(SqlExpression,SqlType)]
getSmallIntColumns (SqlTyped x) = [(x,SqlSmallIntType)]

getIntegerColumns :: SqlTyped SqlInteger -> [(SqlExpression,SqlType)]
getIntegerColumns (SqlTyped x) = [(x,SqlIntegerType)]

getBigIntColumns :: SqlTyped SqlBigInt -> [(SqlExpression,SqlType)]
getBigIntColumns (SqlTyped x) = [(x,SqlBigIntType)]

getFloatColumns :: SqlTyped SqlFloat -> [(SqlExpression,SqlType)]
getFloatColumns (SqlTyped x) = [(x,SqlFloatType)]

getDoubleColumns :: SqlTyped SqlDouble -> [(SqlExpression,SqlType)]
getDoubleColumns (SqlTyped x) = [(x,SqlDoubleType)]

getCharColumns :: SqlTyped SqlChar -> [(SqlExpression,SqlType)]
getCharColumns (SqlTyped x) = [(x,SqlCharType)]

-----------------------------------------------------------------------------

modifyColumns :: (Monad m,Data t) => (SqlExpression -> m SqlExpression) -> t -> m t
modifyColumns f a = everywhereM (mkM (modifySmallIntColumns f)
	`extM` (modifyIntegerColumns f)
	`extM` (modifyBigIntColumns f)
	`extM` (modifyFloatColumns f)
	`extM` (modifyDoubleColumns f)
	`extM` (modifyCharColumns f)) a

modifySmallIntColumns :: Monad m => (SqlExpression -> m SqlExpression) -> SqlTyped SqlSmallInt -> m (SqlTyped SqlSmallInt)
modifySmallIntColumns f (SqlTyped x) = do
	a <- f x
	return (SqlTyped a)

modifyIntegerColumns :: Monad m => (SqlExpression -> m SqlExpression) -> SqlTyped SqlInteger -> m (SqlTyped SqlInteger)
modifyIntegerColumns f (SqlTyped x) = do
	a <- f x
	return (SqlTyped a)

modifyBigIntColumns :: Monad m => (SqlExpression -> m SqlExpression) -> SqlTyped SqlBigInt -> m (SqlTyped SqlBigInt)
modifyBigIntColumns f (SqlTyped x) = do
	a <- f x
	return (SqlTyped a)

modifyFloatColumns :: Monad m => (SqlExpression -> m SqlExpression) -> SqlTyped SqlFloat -> m (SqlTyped SqlFloat)
modifyFloatColumns f (SqlTyped x) = do
	a <- f x
	return (SqlTyped a)

modifyDoubleColumns :: Monad m => (SqlExpression -> m SqlExpression) -> SqlTyped SqlDouble -> m (SqlTyped SqlDouble)
modifyDoubleColumns f (SqlTyped x) = do
	a <- f x
	return (SqlTyped a)

modifyCharColumns :: Monad m => (SqlExpression -> m SqlExpression) -> SqlTyped SqlChar -> m (SqlTyped SqlChar)
modifyCharColumns f (SqlTyped x) = do
	a <- f x
	return (SqlTyped a)

-----------------------------------------------------------------------------

data SqlBinaryOp = SqlAdd | SqlSub | SqlMul | SqlDiv | SqlMod | SqlEq
	| SqlGe | SqlGt | SqlLt | SqlLe | SqlNe | SqlAnd | SqlOr deriving (Eq,Typeable,Data)

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

instance Show SqlBinaryOp where
	showsPrec _ b = showSqlBinaryOp b

sqlBinaryOp :: SqlBinaryOp -> (SqlTyped a -> SqlTyped a -> SqlTyped a)
sqlBinaryOp o = \(SqlTyped x) (SqlTyped y) -> SqlTyped (SqlExpressionBinary o x y)

sqlCmpOp :: SqlBinaryOp -> (SqlTyped a -> SqlTyped a -> SqlTyped Bool)
sqlCmpOp o = \(SqlTyped x) (SqlTyped y) -> SqlTyped (SqlExpressionBinary o x y)

infix 2 `and`
and :: SqlTyped Bool -> SqlTyped Bool -> SqlTyped Bool
and = sqlBinaryOp SqlAnd

infix 2 `or`
or :: SqlTyped Bool -> SqlTyped Bool -> SqlTyped Bool
or = sqlBinaryOp SqlOr

infix 3 `lt`
lt :: SqlTyped a -> SqlTyped a -> SqlTyped Bool
lt = sqlCmpOp SqlLt

infix 3 `gt`
gt :: SqlTyped a -> SqlTyped a -> SqlTyped Bool
gt = sqlCmpOp SqlGt

infix 3 `le`
le :: SqlTyped a -> SqlTyped a -> SqlTyped Bool
le = sqlCmpOp SqlLe

infix 3 `ge`
ge :: SqlTyped a -> SqlTyped a -> SqlTyped Bool
ge = sqlCmpOp SqlGe

infix 3 `eq`
eq :: SqlTyped a -> SqlTyped a -> SqlTyped Bool
eq = sqlCmpOp SqlEq

infix 3 `ne`
ne :: SqlTyped a -> SqlTyped a -> SqlTyped Bool
ne = sqlCmpOp SqlNe

infix 4 `add`
add :: SqlTyped a -> SqlTyped a -> SqlTyped a
add = sqlBinaryOp SqlAdd

infix 4 `sub`
sub :: SqlTyped a -> SqlTyped a -> SqlTyped a
sub = sqlBinaryOp SqlSub

infix 5 `mul`
mul :: SqlTyped a -> SqlTyped a -> SqlTyped a
mul = sqlBinaryOp SqlMul

infix 5 `div`
div :: SqlTyped a -> SqlTyped a -> SqlTyped a
div = sqlBinaryOp SqlDiv

infix 5 `mod`
mod :: SqlTyped Bool -> SqlTyped Bool -> SqlTyped Bool
mod = sqlBinaryOp SqlMod

data SqlAggrOp = SqlCount | SqlSum | SqlMax | SqlMin | SqlAvg | SqlStdDev | SqlVariance deriving (Eq,Typeable,Data)

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

data SqlRelationOp = SqlInnerJoin | SqlLeftJoin | SqlRightJoin | SqlFullJoin | SqlSemiJoin | SqlAntiSemiJoin deriving (Eq,Typeable,Data)

showSqlRelationOp :: SqlRelationOp -> ShowS
showSqlRelationOp SqlInnerJoin = showString "JOIN"
showSqlRelationOp SqlLeftJoin = showString "LEFT JOIN"
showSqlRelationOp SqlRightJoin = showString "RIGHT JOIN"
showSqlRelationOp SqlFullJoin = showString "FULL JOIN"
showSqlRelationOp SqlSemiJoin = showString "WHERE EXISTS"
showSqlRelationOp SqlAntiSemiJoin = showString "WHERE NOT EXISTS"

instance Show SqlRelationOp where
	showsPrec _ r = showSqlRelationOp r

data SqlSetOp = SqlUnion | SqlIntersection | SqlDifference deriving (Eq,Typeable,Data)

showSqlSetOp :: SqlSetOp -> ShowS
showSqlSetOp SqlUnion = showString "UNION"
showSqlSetOp SqlIntersection = showString "INTERSECT"
showSqlSetOp SqlDifference = showString "EXCEPT"

instance Show SqlSetOp where
	showsPrec _ s = showSqlSetOp s

------------------------------------------------------------------------------
-- data types for SQL expressions

type SqlLabel = String
type SqlTag = Int

newtype (Typeable a,Data a) => SqlTyped a = SqlTyped {
	sqlExpression :: SqlExpression
} deriving (Eq,Show,Typeable,Data)

attribute :: SqlColumnName -> SqlTyped a
attribute c = SqlTyped $ SqlExpressionColumnName c

data (Typeable a,Data a) => SqlTable a = SqlTable {
	sqlTableName :: SqlTableName,
	sqlTableType :: a
} deriving (Typeable,Data)

sqlTable :: Data t => String -> t -> SqlTable t
sqlTable n t = SqlTable {
	sqlTableName = n,
	sqlTableType = t
}

data SqlSource = SqlSourceTable SqlTag SqlTableName
	| SqlSourceSelect SqlTag [SqlSelect]
	| SqlSourceRelation SqlTag SqlRelationOp [SqlSelect] [SqlSelect] SqlExpression
	| SqlSourceSet SqlTag SqlSetOp [SqlSelect] [SqlSelect]
	deriving Show

data SqlSelect = SqlSelectSource SqlSource
	| SqlSelectProject SqlTag SqlExpression
	| SqlSelectRestrict SqlExpression
	| SqlSelectGroup SqlExpression
	| SqlSelectOrder SqlExpression
	deriving Show

data SqlInsert = SqlInsert
	deriving Show

data SqlDelete = SqlDelete
	deriving Show

data SqlUpdate = SqlUpdate
	deriving Show

data SqlExpression = SqlExpressionConstant String
	| SqlExpressionTag SqlTag
	| SqlExpressionColumnName SqlColumnName
	| SqlExpressionColumn SqlTag SqlColumnName
	| SqlExpressionBinary SqlBinaryOp SqlExpression SqlExpression
	| SqlExpressionAggr SqlAggrOp SqlExpression
	deriving (Eq,Show,Typeable,Data)

data SqlCommand = SqlCommandSelect [SqlSelect]
	| SqlCommandInsert SqlInsert
	| SqlCommandDelete SqlDelete
	| SqlCommandUpdate SqlUpdate
	deriving Show

data SelectState = SelectState {
	stag :: SqlTag,
	selects :: [SqlSelect]
} deriving Show

data QState = QState {
	qtag :: SqlTag,
	commands :: [SqlCommand]
} deriving Show

newQState :: QState
newQState = QState {
	qtag = 0,
	commands = []
}

type Query a = MonadIO m => StateT QState m a
type Select a = MonadIO m => StateT SelectState m a

------------------------------------------------------------------------------

expression1 :: (Monad m,MonadState SelectState m) => SqlTyped a -> m (SqlTyped a)
expression1 (SqlTyped x) = do
	ss <- getState
	setState $ SelectState {
		stag = stag ss + 1,
		selects = (SqlSelectProject (stag ss) x):selects ss
	}
	return (SqlTyped $ SqlExpressionTag (stag ss))

class Monad m => Project a m where
	project :: a -> m a

instance (Monad m,MonadState SelectState m) => Project (SqlTyped a) m where
	project a = expression1 a 

instance (Monad m,MonadState SelectState m) => Project (SqlTyped a,SqlTyped b) m where
	project (a,b) = do
		a' <- expression1 a 
		b' <- expression1 b
		return (a',b')

instance (Monad m,MonadState SelectState m) => Project (SqlTyped a,SqlTyped b,SqlTyped c) m where
	project (a,b,c) = do
		a' <- expression1 a
		b' <- expression1 b
		c' <- expression1 c
		return (a',b',c')

instance (Monad m,MonadState SelectState m) => Project (SqlTyped a,SqlTyped b,SqlTyped c,SqlTyped d) m where
	project (a,b,c,d) = do
		a' <- expression1 a
		b' <- expression1 b
		c' <- expression1 c
		d' <- expression1 d
		return (a',b',c',d')

instance (Monad m,MonadState SelectState m) => Project (SqlTyped a,SqlTyped b,SqlTyped c,SqlTyped d,SqlTyped e) m where
	project (a,b,c,d,e) = do
		a' <- expression1 a
		b' <- expression1 b
		c' <- expression1 c
		d' <- expression1 d
		e' <- expression1 e
		return (a',b',c',d',e')

------------------------------------------------------------------------------

project1 :: (Monad m,MonadState SelectState m) => SqlReference t -> (t -> SqlTyped a) -> m (SqlTyped a)
project1 ref x = case sqlExpression (x (columns ref)) of
	SqlExpressionColumnName c -> expression1 (SqlTyped $ SqlExpressionColumn (rtag ref) c)
	e -> expression1 (SqlTyped e)

class Monad m => ProjectFrom t a b m | t a -> b where
		projectFrom :: SqlReference t -> a -> m b

instance (Data t,Typeable t,Monad m,MonadState SelectState m)
		=> ProjectFrom t (t -> SqlTyped a) (SqlTyped a) m where
	projectFrom ref a = project1 ref a
	
instance (Data t,Typeable t,Monad m,MonadState SelectState m)
		=> ProjectFrom t (t -> SqlTyped a,t -> SqlTyped b) (SqlTyped a,SqlTyped b) m where
	projectFrom ref (a,b) = do
		a' <- project1 ref a
		b' <- project1 ref b
		return (a',b')

instance (Data t,Typeable t,Monad m,MonadState SelectState m)
		=> ProjectFrom t (t -> SqlTyped a,t -> SqlTyped b,t -> SqlTyped c)
		(SqlTyped a,SqlTyped b,SqlTyped c) m where
	projectFrom ref (a,b,c) = do
		a' <- project1 ref a
		b' <- project1 ref b
		c' <- project1 ref c
		return (a',b',c')

instance (Data t,Typeable t,Monad m,MonadState SelectState m)
		=> ProjectFrom t (t -> SqlTyped a,t -> SqlTyped b,t -> SqlTyped c,t -> SqlTyped d)
		(SqlTyped a,SqlTyped b,SqlTyped c,SqlTyped d) m where
	projectFrom ref (a,b,c,d) = do
		a' <- project1 ref a
		b' <- project1 ref b
		c' <- project1 ref c
		d' <- project1 ref d
		return (a',b',c',d')

instance (Data t,Typeable t,Monad m,MonadState SelectState m)
		=> ProjectFrom t (t -> SqlTyped a,t -> SqlTyped b,t -> SqlTyped c,t -> SqlTyped d,t -> SqlTyped e)
		(SqlTyped a,SqlTyped b,SqlTyped c,SqlTyped d,SqlTyped e) m where
	projectFrom ref (a,b,c,d,e) = do
		a' <- project1 ref a
		b' <- project1 ref b
		c' <- project1 ref c
		d' <- project1 ref d
		e' <- project1 ref e
		return (a',b',c',d',e')

------------------------------------------------------------------------------

restrict1 :: (Monad m,MonadState SelectState m) => SqlTyped Bool -> m ()
restrict1 (SqlTyped x) = do
	ss <- getState
	setState $ SelectState {
		stag = stag ss,
		selects = (SqlSelectRestrict x):selects ss
	}

class Monad m => Restrict a m where
	restrict :: a -> m ()

instance (MonadState SelectState m,Monad m) => Restrict (SqlTyped Bool) m where
	restrict a = restrict1 a

instance (MonadState SelectState m,Monad m) => Restrict (SqlTyped Bool,SqlTyped Bool) m where
	restrict (a,b) = do
		restrict1 a
		restrict1 b

instance (MonadState SelectState m,Monad m) => Restrict (SqlTyped Bool,SqlTyped Bool,SqlTyped Bool) m where
	restrict (a,b,c) = do
		restrict1 a
		restrict1 b
		restrict1 c

instance (MonadState SelectState m,Monad m)
		=> Restrict (SqlTyped Bool,SqlTyped Bool,SqlTyped Bool,SqlTyped Bool) m where
	restrict (a,b,c,d) = do
		restrict1 a
		restrict1 b
		restrict1 c
		restrict1 d

-----------------------------------------------------------------------------

class Show s => Const s t | t -> s where
	const :: s -> t

instance Const String (SqlTyped SqlChar) where
	const s = SqlTyped (SqlExpressionConstant s)

instance Const Int (SqlTyped SqlInteger) where
	const i = SqlTyped (SqlExpressionConstant $ showInt i "")

instance Const Int (SqlTyped SqlSmallInt) where
	const i = SqlTyped (SqlExpressionConstant $ showInt i "")

instance Const Integer (SqlTyped SqlBigInt) where
	const i = SqlTyped (SqlExpressionConstant $ showInt i "")

instance Const Float (SqlTyped SqlFloat) where
	const f = SqlTyped (SqlExpressionConstant $ showFloat f "")

instance Const Double (SqlTyped SqlDouble) where
	const f = SqlTyped (SqlExpressionConstant $ showFloat f "")

-----------------------------------------------------------------------------
-- commands in the QueryMonad

data SqlReference t = SqlReference {
	rtag :: Int,
	columns :: t,
	sql :: [SqlSelect]
} deriving Show

table :: (Data t,Monad m,MonadState SelectState m) => SqlTable t -> m (SqlReference t)
table t = do
	ss <- getState
	setState $ SelectState {
		stag = stag ss + 1,
		selects = SqlSelectSource (SqlSourceTable (stag ss) (sqlTableName t)) : selects ss
	}
	return $ SqlReference {
		rtag = stag ss,
		columns = sqlTableType t
	}

table' :: (Data t,Monad m,MonadState SelectState m) => SqlTable t -> m (SqlReference t)
table' t = do
	ss <- getState
	setState $ SelectState {
		stag = stag ss,
		selects = SqlSelectSource (SqlSourceTable (stag ss) (sqlTableName t)) : selects ss
	}
	return $ SqlReference {
		rtag = stag ss,
		columns = sqlTableType t,
		sql = [SqlSelectSource (SqlSourceTable (stag ss) (sqlTableName t))]
	}

data Join x y = Join {
	left :: x,
	right :: y
} deriving (Data,Typeable,Show,Eq)

incState :: (Monad m,MonadState SelectState m) => Int -> m SelectState
incState i = update (\x -> x { stag = stag x + i })

join' :: (TIR.Union t u v,Monad m,MonadState SelectState m) => SqlRelationOp ->
	SqlReference t -> SqlReference u -> (v -> SqlTyped Bool) -> m (SqlReference v)
join' r t u f = do
	v <- return $ columns t `TIR.union` columns u
	s <- incState 1
	return $ SqlReference {
		rtag = stag s + 1,
		columns = v,
		sql = [(SqlSelectSource . SqlSourceRelation (stag s) r (sql t) (sql u))
			((sqlExpression . f) v)]
	}

join :: (TIR.Union t u v,Monad m,MonadState SelectState m) =>
	SqlReference t -> SqlReference u -> (v -> SqlTyped Bool) -> m (SqlReference v)
join = join' SqlInnerJoin

cross :: (TIR.Union t u v,Monad m,MonadState SelectState m) =>
	SqlReference t -> SqlReference u -> (v -> SqlTyped Bool) -> m (SqlReference v)
cross = join' SqlFullJoin

leftjoin :: (TIR.Union t u v,Monad m,MonadState SelectState m) =>
	SqlReference t -> SqlReference u -> (v -> SqlTyped Bool) -> m (SqlReference v)
leftjoin = join' SqlLeftJoin

rightjoin :: (TIR.Union t u v,Monad m,MonadState SelectState m) =>
	SqlReference t -> SqlReference u -> (v -> SqlTyped Bool) -> m (SqlReference v)
rightjoin = join' SqlRightJoin

semijoin' :: (TIR.Union t u v,Monad m,MonadState SelectState m) => SqlRelationOp -> 
	SqlReference t -> SqlReference u -> (v -> SqlTyped Bool) -> m (SqlReference t)
semijoin' r t u f = do
	v <- return $ columns t `TIR.union` columns u
	s <- incState 1
	return $ SqlReference {
		rtag = stag s,
		columns = (columns t),
		sql = [(SqlSelectSource . SqlSourceRelation (stag s) r (sql t) (sql u))
			((sqlExpression . f) v)]
	}

semijoin :: (TIR.Union t u v,Monad m,MonadState SelectState m) =>
	SqlReference t -> SqlReference u -> (v -> SqlTyped Bool) -> m (SqlReference t)
semijoin = semijoin' SqlSemiJoin

antisemijoin :: (TIR.Union t u v,Monad m,MonadState SelectState m) =>
	SqlReference t -> SqlReference u -> (v -> SqlTyped Bool) -> m (SqlReference t)
antisemijoin = semijoin' SqlAntiSemiJoin

setop :: (Data t,Monad m,MonadState SelectState m) =>
	SqlSetOp -> SqlReference t -> SqlReference t -> m (SqlReference t)
setop op l r = do
	s <- incState 1
	return $ SqlReference {
		rtag = stag s,
		columns = (columns l),
		sql = [SqlSelectSource $ SqlSourceSet (stag s) op (sql l) (sql r)]
	}

union :: (Data t,Monad m,MonadState SelectState m) => SqlReference t -> SqlReference t -> m (SqlReference t)
union = setop SqlUnion

intersection :: (Data t,Monad m,MonadState SelectState m) => SqlReference t -> SqlReference t -> m (SqlReference t)
intersection = setop SqlIntersection

difference :: (Data t,Monad m,MonadState SelectState m) => SqlReference t -> SqlReference t -> m (SqlReference t)
difference = setop SqlDifference

select :: (Monad m,MonadState SelectState m) =>
	(SqlReference t) -> (t -> SqlTyped Bool) -> m (SqlReference t)
select t f = do
	ss <- getState
	setState $ SelectState {
		stag = stag ss,
		selects = (SqlSelectRestrict . sqlExpression . f . columns) t : selects ss
	}
	return $ SqlReference {
		rtag = stag ss,
		columns = columns t,
		sql = (SqlSelectRestrict . sqlExpression . f . columns) t : sql t
	}

project' :: (Monad m,MonadState SelectState m) => (SqlReference t) -> (t -> u) -> m (SqlReference u)
project' t f = do
	ss <- getState
	return $ SqlReference {
		rtag = stag ss,
		columns = (f . columns) t,
		sql = selects ss
	}

relation :: (Data t,Monad m,MonadState SelectState m) => m t -> m (SqlReference t)
relation sel = do
	s1 <- getState
	setState $ SelectState {
		stag = stag s1,
		selects = []
	}
	t <- sel
	s2 <- getState
	setState $ SelectState {
		stag = stag s2 + 1,
		selects = SqlSelectSource (SqlSourceSelect (stag s2) (selects s2)) : selects s1
	}
	return $ SqlReference {
		rtag = stag s2,
		columns = t
	}

-- note: although it is generally better to use class memberships, functions which
-- contain 'run' need to explicity layer the monad-transformers.

query :: Monad m => StateT SelectState (StateT QState m) a -> StateT QState m a
query s = do
	qs <- getState
	(ss,a) <- runST s $ SelectState {
			stag = qtag qs,
			selects = []
		}
	setState $ QState {
		qtag = stag ss,
		commands = SqlCommandSelect (selects ss) : commands qs
	}
	return a

from :: Data t => (t -> SqlTyped a) -> SqlReference t -> SqlTyped a
from x ref = case sqlExpression (x (columns ref)) of
	SqlExpressionColumnName c -> SqlTyped $ SqlExpressionColumn (rtag ref) c
	e -> SqlTyped e

-----------------------------------------------------------------------------

optimiseSelect :: (Monad m,MonadState SelectState m) => m ()
optimiseSelect = do
	s <- getState
	setState $ SelectState {
		stag = stag s,
		selects = concat [optimiseSource (selects s),optimiseProject (selects s),optimiseRestrict (selects s)]
	}

optimiseSource :: [SqlSelect] -> [SqlSelect]
optimiseSource (s0:ss) = case s0 of
	SqlSelectSource x -> SqlSelectSource x : optimiseSource ss
	_ -> optimiseSource ss

optimiseProject :: [SqlSelect] -> [SqlSelect]
optimiseProject (p0:ps) = case p0 of
	SqlSelectProject t x -> SqlSelectProject t x : optimiseProject ps
	_ -> optimiseProject ps

optimiseRestrict :: [SqlSelect] -> [SqlSelect]
optimiseRestrict (r0:rs) = case r0 of
	SqlSelectRestrict x -> SqlSelectRestrict x : optimiseRestrict rs
	_ -> optimiseRestrict rs
