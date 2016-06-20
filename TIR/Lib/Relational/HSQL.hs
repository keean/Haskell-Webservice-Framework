{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- hsql.hs 
-- Copyright (C) 2002 Keean Schupke

module Lib.Relational.HSQL where

import Numeric
import Char
import Data.FiniteMap
import Data.Set hiding (map)

import Lib.Monad.StateT
import Lib.Monad.MonadState
import Lib.Monad.MonadIO
import Lib.ODBC.Types
import Lib.ODBC.HIODBC
import Lib.TIR.Control as Ctrl
import Lib.TIR.Logic as Logic
import Lib.TIR.Peano as Peano
import Lib.TIR.HList as HList
import Lib.TIR.HType as HType
import Lib.TIR.HRecord as HRecord
import Lib.TIR.HRecordGHC as HRecordGHC
import Lib.Relational.Types

------------------------------------------------------------------------------
-- map attributes in table schema to attributed in a relation.

data AttrToTyped = AttrToTyped
instance Apply AttrToTyped (Attribute a b) (SqlTyped a b) where
	apply AttrToTyped (Attribute ad) = SqlTyped (SqlExpressionColumn ad)

class RMapValues AttrToTyped r r' => AttributeToTyped r r' | r -> r' where
	attributeToTyped :: r -> r'
instance RMapValues AttrToTyped r r' => AttributeToTyped r r' where
	attributeToTyped = rMapValues AttrToTyped

------------------------------------------------------------------------------
-- convert HList into [] loosing phantom type info. 

data TypedToExpr = TypedToExpr
instance Apply TypedToExpr (SqlTyped a b) SqlExpression where
	apply TypedToExpr (SqlTyped e) = e

class Record r => TypedToExpression r where
	typedToExpression :: r -> [SqlExpression]
instance (RValues r l,HMap TypedToExpr l l',HOccursMany l' SqlExpression) => TypedToExpression r where
	typedToExpression = hOccursMany . hMap TypedToExpr . rValues 

------------------------------------------------------------------------------
-- set all attribute tags in a record to new value.

data SetTags = SetTags Int
instance Apply SetTags (SqlTyped a b) (SqlTyped a b) where
	apply (SetTags i) (SqlTyped (SqlExpressionColumn (ad :: AttrDetails)))
		= SqlTyped (SqlExpressionColumn (ad { attrTags = i:attrTags ad }))
	apply (SetTags _) x = x

class Record r => Relabel r where
	relabel :: r -> Int -> r
instance RMapValues SetTags r r => Relabel r where
	relabel r i = rMapValues (SetTags i) r

------------------------------------------------------------------------------
-- Create a relation from a table schema

table :: (MonadState Int m,AttributeToTyped r r',TypedToExpression r')
	=> Table r -> m (Relation (SqlTypeRep MultiRow r'))
table t = do
	i <- update (+1) -- assign unique label to relation
	let cols = (attributeToTyped . sqlTableType) t
		in return $ Relation (SqlTypeRep MultiRow cols) $ sqlRelation {
			rTag = i,
			rSource = (SqlSourceTable . sqlTableName) t,
			rProjectList = typedToExpression cols
		}

------------------------------------------------------------------------------
-- Restrict a relation

restrict :: Monad m => Relation (SqlTypeRep w r) -> (r -> SqlTyped Bool SqlChar) -> m (Relation (SqlTypeRep w r))
restrict (Relation (SqlTypeRep rows cols) r) f = return $ Relation (SqlTypeRep rows cols) $ r {
	rRestrictList = (sqlExpression . f) cols : rRestrictList r
}

------------------------------------------------------------------------------
-- project a relation

project :: (Monad m,RProject r p r',TypedToExpression r')
	=> Relation (SqlTypeRep w r) -> p -> m (Relation (SqlTypeRep w r'))
project (Relation (SqlTypeRep rows cols) r) p = let x = rProject cols p
	in return $ Relation (SqlTypeRep rows x) $ r { rProjectList = typedToExpression x }

------------------------------------------------------------------------------
-- extend a relation

extend :: (Monad m,HSnoc r (k,SqlTyped a b) r',TypedToExpression r')
	=> Relation (SqlTypeRep w r) -> (r -> SqlTyped a b) -> k -> m (Relation (SqlTypeRep w r'))
extend (Relation (SqlTypeRep rows cols) r) f k = let x = hSnoc cols (k,f cols)
	in return $ Relation (SqlTypeRep rows x) $ r { rProjectList = typedToExpression x }

------------------------------------------------------------------------------
-- limit a relation

limit :: (Monad m,Peano.NotNegative n,Peano.Eq n Peano.One t,Conditional t SingleRow MultiRow w)
	=> Relation (SqlTypeRep MultiRow r) -> n -> m (Relation (SqlTypeRep w r))
limit (Relation (SqlTypeRep MultiRow cols) r) l =
	return $ Relation (SqlTypeRep (cond (l `Peano.eq` Peano.one) SingleRow MultiRow) cols) $ r {
		rLimit = Just (reflectNumber l) }	

------------------------------------------------------------------------------
-- order a relation

order :: Monad m => Relation (SqlTypeRep w r) -> (r -> SqlTyped a b) -> m (Relation (SqlTypeRep w r))
order (Relation (SqlTypeRep rows cols) r) c = return $ Relation (SqlTypeRep rows cols) $ r {
	rOrderList = (sqlExpression . c) cols:rOrderList r
}

------------------------------------------------------------------------------
-- join relations

join :: (MonadState Int m,HAppend r r' r'',Relabel r,Relabel r',TypedToExpression r'',IntersectionRowReps w w' w'')
	=> SqlRelationOp -> Relation (SqlTypeRep w r) -> Relation (SqlTypeRep w' r') -> (r'' -> SqlTyped Bool SqlChar)
	-> m (Relation (SqlTypeRep w'' r''))
join rOp (Relation (SqlTypeRep w0 c0) r0) (Relation (SqlTypeRep w1 c1) r1) on = do
	i <- update (+1)
	let j = hAppend (relabel c0 (rTag r0)) (relabel c1 (rTag r1))
		in return $ Relation (SqlTypeRep (intersectionRowReps w0 w1) j) $ sqlRelation {
			rTag = i,
			rSource = SqlSourceRelation rOp r0 r1 (sqlExpression $ on j),	
			rProjectList = typedToExpression j
		}

------------------------------------------------------------------------------
-- setop relations

union :: (MonadState Int m,Relabel r,TypedToExpression r,RValues r v,RValues r' v)
	=> Relation (SqlTypeRep w r) -> Relation (SqlTypeRep w' r') -> m (Relation (SqlTypeRep MultiRow r))
union (Relation (SqlTypeRep _ c0) r0) (Relation (SqlTypeRep _ _) r1) = do
	i <- update (+1)
	let j = relabel c0 (rTag r0) in return $ Relation (SqlTypeRep MultiRow j) $ sqlRelation {
		rTag = i,
		rSource = SqlSourceSet SqlUnion r0 r1,
		rProjectList = typedToExpression j
	}

intersect :: (MonadState Int m,IntersectionRowReps w w' w'',Relabel r,
	TypedToExpression r,RValues r v,RValues r' v)
	=> Relation (SqlTypeRep w r) -> Relation (SqlTypeRep w' r') -> m (Relation (SqlTypeRep w'' r))
intersect (Relation (SqlTypeRep w0 c0) r0) (Relation (SqlTypeRep w1 _) r1) = do
	i <- update (+1)
	let j = relabel c0 (rTag r0) in return $ Relation (SqlTypeRep (intersectionRowReps w0 w1) j) $ sqlRelation {
		rTag = i,
		rSource = SqlSourceSet SqlIntersection r0 r1,
		rProjectList = typedToExpression j
	}

except :: (MonadState Int m,Relabel r,TypedToExpression r,RValues r v,RValues r' v)
	=> Relation (SqlTypeRep w r) -> Relation (SqlTypeRep w' r') -> m (Relation (SqlTypeRep w r))
except (Relation (SqlTypeRep w0 c0) r0) (Relation (SqlTypeRep _ _) r1) = do
	i <- update (+1)
	let j = relabel c0 (rTag r0) in return $ Relation (SqlTypeRep w0 j) $ sqlRelation {
		rTag = i,
		rSource = SqlSourceSet SqlDifference r0 r1,
		rProjectList = typedToExpression j
	}

------------------------------------------------------------------------------
-- what happens to relations rows when joining
-- note: intersection behaves this way as well,
--   union always produces MultiRow, difference 
--   just inherits the left arguments state IE
--   (SingeRow - any -> SingleRow | MultiRow - any -> MultiRow)

class IntersectionRowReps w w' w'' | w w' -> w'' where
	intersectionRowReps :: w -> w' -> w''
instance IntersectionRowReps SingleRow SingleRow SingleRow where
	intersectionRowReps _ _ = SingleRow
instance IntersectionRowReps SingleRow MultiRow SingleRow where
	intersectionRowReps _ _ = SingleRow
instance IntersectionRowReps MultiRow SingleRow SingleRow where
	intersectionRowReps _ _ = SingleRow
instance IntersectionRowReps MultiRow MultiRow MultiRow where
	intersectionRowReps _ _ = MultiRow

------------------------------------------------------------------------------
-- convert a relation into an expression (for IN clauses)

class RelationToExpr w r a | w r -> a where
	relation :: Relation (SqlTypeRep w r) -> a
instance RelationToExpr MultiRow (HCons (k,SqlTyped a b) HNil) (SqlTyped [a] [b]) where
	relation (Relation _ r) = SqlTyped (SqlExpressionRelation r)
instance RelationToExpr SingleRow (HCons (k,SqlTyped a b) HNil) (SqlTyped a b) where
	relation (Relation _ r) = SqlTyped (SqlExpressionRelation r)

------------------------------------------------------------------------------
-- select command

doSelect :: (Show r,HSize r n,RValues r v,ToHList v v',RNewValues r v' r')
	=> Relation (SqlTypeRep w r) -> Query [r']
doSelect (Relation (SqlTypeRep _ c) r) = do
	-- closeCursor 
	sqlDo (showSqlRelation r "")
	rows <- getRowsAsListOfLists
	return (selLp c rows)

selLp :: (RValues r v,ToHList v v',RNewValues r v' r') => r -> [[Maybe String]] -> [r']
selLp r (s0:ss) = rNewValues r (toHList (rValues r) s0) : selLp r ss
selLp _ _ = []

class ToHList r r' | r -> r' where
	toHList :: r -> [Maybe String] -> r'
instance ToHList HNil HNil where
	toHList _ _ = HNil
instance (ToHList r r',FromSqlType b a) => ToHList (HCons (SqlTyped a b) r) (HCons (Maybe a) r') where
	toHList (HCons _ r) [] = HCons Nothing (toHList r [])
	toHList (HCons a r) (s0:ss) = case s0 of  
		Just s -> HCons (fromSqlType a s) (toHList r ss)
		_ -> HCons Nothing (toHList r ss)
		
------------------------------------------------------------------------------
-- update command
-- 
-- update animalTable (\r -> AnimalName .=. const "Flossy" :*: AnimalType .=. r!AnimalType `SQL.add` 1 :*: HNil)
-- 	(\r -> r!AnimalId `SQL.elem` r3)

doUpdate :: (HNull u Logic.AllFalse,AttributeToTyped t t',
	RKeys u k,RProjectOrder2 t' k u,TypedToExpression u)
	=> Table t -> (t' -> u) -> (t' -> SqlTyped Bool SqlChar) -> Query ()
doUpdate t u f = let
	cols = (attributeToTyped . sqlTableType) t
	uk = (typedToExpression . rProjectOrder2 cols . rKeys) (u cols)
	uv = typedToExpression (u cols)
	wh = (sqlExpression . f) cols
	in do
		-- closeCursor
		sqlDo (showUpdate (sqlTableName t) uk uv [wh] "")

showUpdate :: SqlTableName -> [SqlExpression] -> [SqlExpression] -> [SqlExpression] -> ShowS
showUpdate n k v x = showString "UPDATE " . showString n . showString " SET "
	. showAssignments k v . (case x of
		(_:_) -> showString " WHERE " . showRestrictList x
		_ -> id)

------------------------------------------------------------------------------
-- insert command (need to handle "insert into (cols) select" form

doInsertC :: (HNull u Logic.AllFalse,AttributeToTyped t t',
	RKeys u k,RProjectOrder2 t' k u,TypedToExpression u)
	=> Table t -> u -> Query ()
doInsertC t u = let
	cols = (attributeToTyped . sqlTableType) t
	uk = (typedToExpression . rProjectOrder2 cols . rKeys) u
	uv = typedToExpression u
	in do
		-- closeCursor
		sqlDo (showInsertC (sqlTableName t) uk uv "")

showInsertC :: SqlTableName -> [SqlExpression] -> [SqlExpression] -> ShowS
showInsertC n k v = showString "INSERT INTO " . showString n . showString " ("
	. showExpressionList k . showString ") VALUES("
	. showExpressionList v . showChar ')'

------------------------------------------------------------------------------
-- insert command (need to handle "insert into (cols) select" form

doInsertR :: (HNull u Logic.AllFalse,RValues u' v,		-- explicit constraints
	RValues r v,AttributeToTyped t t',RProjectOrder2 t' u u',			-- implicit constraints
	TypedToExpression u')
	=> Relation (SqlTypeRep w r) -> Table t -> u -> Query ()

doInsertR (Relation _ r) t c = let
	cols = (attributeToTyped . sqlTableType) t
	uk = (typedToExpression . rProjectOrder2 cols) c
	in do
		-- closeCursor
		sqlDo (showInsertR (sqlTableName t) uk r "")

showInsertR :: SqlTableName -> [SqlExpression] -> SqlRelation -> ShowS
showInsertR n k r = showString "INSERT INTO " . showString n . showString " ("
	. showExpressionList k . showString ") " . showSqlRelation r

------------------------------------------------------------------------------
-- delete command

doDelete :: AttributeToTyped t t' => Table t -> (t' -> SqlTyped Bool SqlChar) -> Query ()
doDelete t f = let
	cols = (attributeToTyped . sqlTableType) t
	wh = (sqlExpression . f) cols
	in do
		-- closeCursor
		sqlDo (showDelete (sqlTableName t) [wh] "")

showDelete :: SqlTableName -> [SqlExpression] -> ShowS
showDelete n x = showString "DELETE FROM " . showString n . (case x of
	(_:_) -> showString " WHERE " . showRestrictList x
	_ -> id)

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- connect to DB using supplied connection function

dbConnectWith :: VerifyTables t => (SqlHandle () -> IO ()) -> t -> Query () -> IO ()
dbConnectWith confn tabs query = confn (do { _ <- runST (do verifyTables tabs ; query) 0; return () })

------------------------------------------------------------------------------
-- Check DB tables against Haskell Schema

class VerifyTables t where
	verifyTables :: t -> SqlHandle ()
instance VerifyTables HNil where
	verifyTables _ = return ()
instance (VerifyTables ts,ToColTypes t0) => VerifyTables (HCons (Table t0) ts) where
	verifyTables (HCons t0 ts) = do
		showTables "" "" "" ""
		tableInfoNames <- getColNameList
		tableNameList <- getTableNames tableInfoNames
		closeCursor
		verifyTable t0 (mkSet tableNameList)
		verifyTables ts
		return ()

getTableNames :: ColNameList -> SqlHandle [String]
getTableNames clist = do
	status <- fetch
	case status of
		SqlSuccess -> do
			row <- getRowAsFM clist
			tableName <- return (lookupWithDefaultFM row Nothing "TABLE_NAME")
			rows <- getTableNames clist
			case tableName of
				Just tn -> return (tn:rows)
				_ -> return rows
		_ -> return []

verifyTable :: ToColTypes r => Table r -> Set SqlTableName ->  SqlHandle ()
verifyTable tab tableSet = case (map toLower $ sqlTableName tab) of
	tn	| tn `elementOf` tableSet -> do
			showColumns "" "" tn ""
			clist <- getColNameList
			colMap <- getColTypeFM clist
			closeCursor
			colTypes <- return $ toColTypes $ sqlTableType tab
			vt colMap tn colTypes
		| otherwise -> do
			colTypes <- return $ toColTypes $ sqlTableType tab
			createTable tn colTypes

class Record r => ToColTypes r where
	toColTypes :: r -> [Attr]
instance ToColTypes HNil where
	toColTypes _ = []
instance (SqlTypeable c,ToColTypes r) => ToColTypes (HCons (k,Attribute d c) r) where
	toColTypes (HCons (_,Attribute ad :: Attribute domain colType) r) = Attr {
			attrSqlType = toSqlTypeEnum (undefined :: colType),
			attrDetails = ad
		}:toColTypes r

createTable :: SqlTableName -> [Attr] -> SqlHandle ()
createTable tableName cs = do
	sqlDo ((showString "CREATE TABLE "
		. showString tableName
		. showString "( "
		. createColumns cs) " )")

createColumns :: [Attr] -> ShowS
createColumns (a:cs@(_:_)) = showString (attrName $ attrDetails a)
	. showChar ' '
	. showAttr a
	. showString ", "
	. createColumns cs
createColumns [a] = showString (attrName $ attrDetails a)
	. showChar ' '
	. showAttr a
createColumns _ = id

vt :: FiniteMap SqlColumnName String -> SqlTableName -> [Attr] -> SqlHandle ()
vt cm tn (a:cs) = do
	case lookupFM cm (map toLower (attrName ad)) of
		Just t
			| tp == (attrSqlType a) -> vt cm tn cs
			| otherwise -> ioUserException ((showString "Table "
				. showString tn
				. showString " column "
				. showString (attrName ad)
				. showString " has type "
				. showSqlType tp
				. showString " in database: expected type "
				. showSqlType (attrSqlType a)) ".")
			where
				tp = readSqlType t
		Nothing -> do
			sqlDo ((showString "ALTER TABLE "
				. showString tn
				. showString " ADD COLUMN "
				. showString (attrName $ attrDetails a)) "")
			vt cm tn cs
	where

		ad :: AttrDetails
		ad = attrDetails a

vt _ _ [] = return ()

readSqlType :: String -> SqlType
readSqlType s = case readSigned readDec s of
	((i,_):_) -> toEnum i
	_ -> SqlUnknownType

getColTypeFM :: ColNameList -> SqlHandle (FiniteMap SqlColumnName String)
getColTypeFM clist = do
	status <- fetch
	case status of
		SqlSuccess -> do
			row <- getRowAsFM clist
			colName <- return (lookupWithDefaultFM row Nothing "COLUMN_NAME")
			colType <- return (lookupWithDefaultFM row Nothing "DATA_TYPE")
			rows <- getColTypeFM clist
			case colName of
				Just cn -> case colType of
					Just ct -> return (addToFM rows (map toLower cn) ct)
					_ -> return rows
				_ -> return rows
		_ -> return emptyFM

