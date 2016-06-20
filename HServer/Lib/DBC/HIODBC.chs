-- Haskell IODBC interface
-- Copyright (C) 2002 Keean Schupke
-- note: do not use single quotes in function names for c2hs

module Lib.DBC.HIODBC (connectToDb,safeAllocEnv,safeAllocConnect,safeConnect,safeAllocStmt,safeAllocBuffer,
	prepare,execute,numResultCols,describeCol,fetch,getDataAsString,sqlQuery,getRowAsFM,sqlStmt,
	
	SqlEnv,SqlDbc,SqlStmt,SqlStBuf,SqlDbcBuf,SqlPointer,SqlSmallInt,SqlUSmallInt,SqlInteger,SqlUInteger,
	SqlBuffer,SqlDsn,SqlUid,SqlAuth,SqlCon,SqlCmd,SqlCol,SqlRow,SqlColName,SqlNameSz,SqlColPrecision,
	SqlColScale,SqlBufferSize,SqlDataAttr,DataString,ColNameList,SqlResult(..),SqlType(..),
	SqlNullable(..),SqlDataAttrEnum(..),SqlColMap) where

import IOExts (unsafeInterleaveIO)
import CTypes
import CString
import MarshalAlloc
import Ptr
import Exception
import IO
import FiniteMap

{#context lib="libiodbc"#}

{#pointer SQLHENV_PTR as SqlEnv#}
{#pointer SQLHDBC_PTR as SqlDbc#}
{#pointer SQLHSTMT_PTR as SqlStmt#}
{#pointer SQLPOINTER as SqlPointer#}
{#pointer SQLSMALLINT_PTR as SqlSmallInt#}
{#pointer SQLUSMALLINT_PTR as SqlUSmallInt#}
{#pointer SQLINTEGER_PTR as SqlInteger#}
{#pointer SQLUINTEGER_PTR as SqlUInteger#}
newtype SqlBuffer = MkBuffer (SqlPointer,{#type SQLINTEGER#})

type SqlDsn = String
type SqlUid = String
type SqlAuth = String
type SqlCon = (SqlDsn,SqlUid,SqlAuth)
type SqlCmd = String
type SqlCol = Int
type SqlRow = Int
type SqlColName = String
type SqlNameSz = Int
type SqlColPrecision = Int
type SqlColScale = Int
type SqlBufferSize = Int
type SqlDataAttr = Int
type SqlStBuf = (SqlStmt,SqlBuffer)
type SqlDbcBuf = (SqlDbc,SqlBuffer)

type DataString = String

{#enum SqlResult {underscoreToCase} deriving (Eq,Show)#}
{#enum SqlType {underscoreToCase} deriving (Eq,Show)#}
{#enum SqlNullable {underscoreToCase} deriving (Eq,Show)#}
{#enum SqlDataAttrEnum {underscoreToCase} deriving (Eq,Show)#}

maxSqlIdentifierLength :: Int
maxSqlIdentifierLength = 256 -- ANSI SQL MAX IDENTIFIER LENGTH = 128

maxSqlErrorStringLength :: Int
maxSqlErrorStringLength = 256

ifFail :: CInt -> (SqlResult -> IO SqlResult) -> IO SqlResult
ifFail status fl = case toEnum (fromIntegral status) of
	s	| status < 0 -> fl s
		| otherwise -> return s

---------------------------------------------------------------------------

safeAllocEnv :: (SqlEnv -> IO a) -> IO a
safeAllocEnv doWith = Exception.bracket ( do
		env <- {#call unsafe sqlNewEnv#}
		status <- {#call unsafe sqlIsValidEnv#} env
		ifFail status (\s -> fail ((showString " Could not malloc in sqlNewEnv (" . shows s) ")")) 
		return env) ({#call unsafe sqlDeleteEnv#}) (\env -> Exception.bracket ( do
			status <- {#call unsafe sqlAllocEnv#} env
			ifFail status (\s -> fail ((showString " Bad status returned by sqlAllocEnv (" . shows s) ")")))
			(\_ -> {#call unsafe sqlFreeEnv#} env) (\_ -> doWith env))

safeAllocConnect :: SqlEnv -> (SqlDbc -> IO a) -> IO a
safeAllocConnect env doWith = Exception.bracket ( do
		dbc <- {#call unsafe sqlNewConnect#}
		status <- {#call unsafe sqlIsValidConnect#} dbc
		ifFail status (\s -> fail ((showString " Could not malloc in sqlNewConnect (" . shows s) ")"))
		return dbc) ({#call unsafe sqlDeleteConnect#}) (\dbc -> Exception.bracket ( do
			status <- {#call unsafe sqlAllocConnect#} env dbc
			ifFail status (\s -> fail((showString " Bad status returned by sqlAllocConnect" . shows s) ")")))
			(\_ -> {#call unsafe sqlFreeConnect#} dbc) (\_ -> doWith dbc))

safeConnect :: SqlDbc -> SqlCon -> (SqlDbc -> IO a) -> IO a
safeConnect dbc (dsn,uid,auth) doWith = Exception.bracket (
		Exception.bracket (newCStringLen dsn) (\(dsnS,_) -> free dsnS) (\(dsnS,dsnL) -> 
		Exception.bracket (newCStringLen uid) (\(uidS,_) -> free uidS) (\(uidS,uidL) -> 
		Exception.bracket (newCStringLen auth) (\(authS,_) -> free authS) (\(authS,authL) -> do
			status <- {#call unsafe sqlConnect#} dbc dsnS (fromIntegral dsnL) uidS (fromIntegral uidL) authS (fromIntegral authL)
			ifFail status (\s -> fail ((showString " Bad status returned by sqlConnect (" . shows s) ")"))))))
		(\_ -> {#call unsafe sqlDisconnect#} dbc) (\_ -> doWith dbc)

safeAllocStmt :: SqlDbc -> (SqlStmt -> IO a) -> IO a
safeAllocStmt dbc doWith = Exception.bracket ( do
		stmt <- {#call unsafe sqlNewStmt#}
		status <- {#call unsafe sqlIsValidStmt#} stmt
		ifFail status (\s -> fail ((showString " Could not malloc in sqlNewStmt (" . shows s) ")"))
		return stmt) ({#call unsafe sqlDeleteStmt#}) (\stmt -> Exception.bracket ( do 
			status <- {#call unsafe sqlAllocStmt#} dbc stmt
			ifFail status (\s -> fail ((showString " Bad status returned by sqlAllocStmt (" . shows s) ")")))
			(\_ -> {#call unsafe sqlFreeStmt#} stmt) (\_ -> doWith stmt))

safeAllocBuffer :: SqlBufferSize -> (SqlBuffer -> IO a) -> IO a
safeAllocBuffer size doWith = case fromIntegral size of
	sz -> Exception.bracket ( do
		buf <- {#call unsafe sqlNewBuffer#} sz
		status <- {#call unsafe sqlIsValidBuffer#} buf
		ifFail status (\s -> fail ((showString " Could not malloc in sqlNewBuffer (" . shows s) ")"))
		return buf) ({#call unsafe sqlDeleteBuffer#}) (\buf -> do doWith (MkBuffer (buf,sz)))

prepare :: SqlStmt -> SqlCmd -> IO SqlResult
prepare stmt cmd = Exception.bracket (newCStringLen cmd) (\(cmdS,_) -> free cmdS) (\(cmdS,cmdL) -> do
		status <- {#call unsafe sqlPrepare#} stmt cmdS (fromIntegral cmdL)
		ifFail status (\s -> fail ((showString " Bad status returned by sqlPrepare (" . shows s) ")")))

execute :: SqlStmt -> IO SqlResult
execute stmt = do
	status <- {#call unsafe sqlExecute#} stmt
	ifFail status (\s -> fail ((showString " Bad status returned by sqlExecute (" . shows s) ")"))

numResultCols :: SqlStmt -> IO SqlCol
numResultCols stmt = Exception.bracket ( do
		colsPtr <- {#call unsafe sqlNewSmallInt#}
		status <- {#call unsafe sqlIsValidSmallInt#} colsPtr
		ifFail status (\s -> fail ((showString " Could not malloc colsPtr (" . shows s) ")"))
		return colsPtr) ({#call unsafe sqlDeleteSmallInt#}) (\colsPtr -> do
			status <- {#call unsafe sqlNumResultCols#} stmt colsPtr
			ifFail status (\s -> fail ((showString " Bad status returned by sqlNumResultCols (" . shows s) ")"))
			cols <- {#call unsafe sqlGetSmallInt#} colsPtr
			return (fromIntegral cols))

describeCol :: SqlStmt -> SqlCol -> IO (SqlColName,SqlType,SqlColPrecision,SqlColScale,SqlNullable)
describeCol stmt col = Exception.bracket ( do
	colNamePtr <- {#call unsafe sqlNewString#} (fromIntegral maxSqlIdentifierLength)
	status <- {#call unsafe sqlIsValidString #} colNamePtr
	ifFail status (\s -> fail ((showString " Could not malloc colNamePtr (" . shows s) ")"))
	return colNamePtr) ({#call unsafe sqlDeleteString#}) (\colNamePtr -> Exception.bracket ( do
		colTypePtr <- {#call unsafe sqlNewSmallInt#}
		status <- {#call unsafe sqlIsValidSmallInt#} colTypePtr
		ifFail status (\s -> fail ((showString " Could not malloc colTypePtr (" . shows s) ")"))
		return colTypePtr) ({#call unsafe sqlDeleteSmallInt#}) (\colTypePtr -> Exception.bracket ( do
			colPrecisionPtr <- {#call unsafe sqlNewUInteger#}
			status <- {#call unsafe sqlIsValidUInteger#} colPrecisionPtr
			ifFail status (\s -> fail ((showString " Could not malloc colPrecisionPtr (" . shows s) ")"))
			return colPrecisionPtr) ({#call unsafe sqlDeleteUInteger#}) (\colPrecisionPtr -> Exception.bracket ( do
				colScalePtr <- {#call unsafe sqlNewSmallInt#}
				status <- {#call unsafe sqlIsValidSmallInt#} colScalePtr
				ifFail status (\s -> fail ((showString " Could not malloc colScalePtr (" . shows s) ")"))
				return colScalePtr) ({#call unsafe sqlDeleteSmallInt#}) (\colScalePtr -> Exception.bracket ( do
					colNullablePtr <- {#call unsafe sqlNewSmallInt#}
					status <- {#call unsafe sqlIsValidSmallInt#} colNullablePtr
					ifFail status (\s -> fail ((showString " Could not malloc colNullablePtr" . shows s) ")"))
					return colNullablePtr) ({#call unsafe sqlDeleteSmallInt#}) (\colNullablePtr -> do
						status <- ({#call unsafe sqlDescribeCol#} stmt (fromIntegral col) colNamePtr
							(fromIntegral maxSqlIdentifierLength) colTypePtr colPrecisionPtr colScalePtr
							colNullablePtr)
						ifFail status (\s -> fail ((showString " Bad status returned by sqlDescribeCol (" . shows s) ")"))
						colName <- peekCString colNamePtr
						colType <- {#call unsafe sqlGetSmallInt#} colTypePtr
						colPrecision <- {#call unsafe sqlGetUInteger#} colPrecisionPtr
						colScale <- {#call unsafe sqlGetSmallInt#} colScalePtr
						colNullable <- {#call unsafe sqlGetSmallInt#} colNullablePtr
						return (colName,toEnum (fromIntegral colType),fromIntegral colPrecision,fromIntegral colScale,toEnum (fromIntegral colNullable)))))))

fetch :: SqlStBuf -> IO SqlResult
fetch (stmt,_) = do
	status <- {#call unsafe sqlFetch#} stmt
	ifFail status (\s -> fail ((showString " Bad status returned by sqlFetch (" . shows s) ")"))

getDataAsString :: SqlStBuf -> SqlCol -> IO (SqlDataAttr,DataString)
getDataAsString stbuf@(stmt,(MkBuffer (bufPtr,bufSz))) col = do
	(s,dataAttr) <- Exception.bracket ( do
		dataAttrPtr <- {#call unsafe sqlNewInteger#}
		status <- {#call unsafe sqlIsValidInteger#} dataAttrPtr
		ifFail status (\s -> fail ((showString " Could not malloc dataAttrPtr (" . shows s) ")"))
		return dataAttrPtr) ({#call unsafe sqlDeleteInteger#}) (\dataAttrPtr -> do
			status <- {#call unsafe sqlGetData#} stmt (fromIntegral col) (fromIntegral (fromEnum SqlCharType)) bufPtr bufSz dataAttrPtr
			s <- ifFail status (\s -> fail ((showString " Bad status returned by getDataAsString (" . shows s) ")"))
			dataAttr <- {#call unsafe sqlGetInteger#} dataAttrPtr
			return (s,dataAttr))
	if dataAttr > 0
		then if s == SqlSuccessWithInfo
			then do
				dataStr <- peekCStringLen (castPtr bufPtr,fromIntegral bufSz)
				(fdataAttr,fdataStr) <- unsafeInterleaveIO (getDataAsString stbuf col)
				return (fromIntegral dataAttr,dataStr++fdataStr)
			else do
				dataStr <- peekCStringLen (castPtr bufPtr,fromIntegral dataAttr)
				return (fromIntegral dataAttr,dataStr)
		else return (fromIntegral dataAttr,"")

getStmtError :: SqlEnv -> SqlDbc -> SqlStmt -> IO String
getStmtError env dbc stmt = Exception.bracket ( do
	statBuf <- {#call unsafe sqlNewString#} (fromIntegral maxSqlErrorStringLength)
	status <- {#call unsafe sqlIsValidString#} statBuf
	ifFail status (\s -> fail ((showString " Could not malloc status buffer (" . shows s) ")"))
	return statBuf) ({#call unsafe sqlDeleteString#}) (\statBuf -> Exception.bracket ( do
		errBuf <- {#call unsafe sqlNewString#} (fromIntegral maxSqlErrorStringLength)
		status <- {#call unsafe sqlIsValidString#} errBuf
		ifFail status (\s -> fail ((showString " Could not malloc error buffer (" . shows s) ")"))
		return errBuf) ({#call unsafe sqlDeleteString#}) (\errBuf -> do
			status <- {#call unsafe sqlStmtError#} env dbc stmt statBuf errBuf (fromIntegral maxSqlErrorStringLength)
			if toEnum (fromIntegral status) == SqlSuccess
				then do bstr <- peekCString errBuf
					bsts <- peekCString statBuf
					next <- getStmtError env dbc stmt
					return ((showString bstr . showString ", SQLSTATE=" . showString bsts . showChar '\n' . showString next) "")
				else return ""))

getConnectError :: SqlEnv -> SqlDbc -> IO String
getConnectError env dbc = Exception.bracket ( do
	statBuf <- {#call unsafe sqlNewString#} (fromIntegral maxSqlErrorStringLength)
	status <- {#call unsafe sqlIsValidString#} statBuf
	ifFail status (\s -> fail ((showString " Could not malloc status buffer (" . shows s) ")"))
	return statBuf) ({#call unsafe sqlDeleteString#}) (\statBuf -> Exception.bracket ( do
		errBuf <- {#call unsafe sqlNewString#} (fromIntegral maxSqlErrorStringLength)
		status <- {#call unsafe sqlIsValidString#} errBuf
		ifFail status (\s -> fail ((showString " Could not malloc error buffer (" . shows s) ")"))
		return errBuf) ({#call unsafe sqlDeleteString#}) (\errBuf -> do
			status <- {#call unsafe sqlConnectError#} env dbc statBuf errBuf (fromIntegral maxSqlErrorStringLength)
			if toEnum (fromIntegral status) == SqlSuccess
				then do bstr <- peekCString errBuf
					bsts <- peekCString statBuf
					next <- getConnectError env dbc
					return ((showString bstr . showString ", SQLSTATE=" . showString bsts . showChar '\n' . showString next) "")
				else return ""))

getEnvError :: SqlEnv -> IO String
getEnvError env = Exception.bracket ( do
	statBuf <- {#call unsafe sqlNewString#} (fromIntegral maxSqlErrorStringLength)
	status <- {#call unsafe sqlIsValidString#} statBuf
	ifFail status (\s -> fail ((showString " Could not malloc status buffer (" . shows s) ")"))
	return statBuf) ({#call unsafe sqlDeleteString#}) (\statBuf -> Exception.bracket ( do
		errBuf <- {#call unsafe sqlNewString#} (fromIntegral maxSqlErrorStringLength)
		status <- {#call unsafe sqlIsValidString#} errBuf
		ifFail status (\s -> fail ((showString " Could not malloc error buffer (" . shows s) ")"))
		return errBuf) ({#call unsafe sqlDeleteString#}) (\errBuf -> do
			status <- {#call unsafe sqlEnvError#} env statBuf errBuf (fromIntegral maxSqlErrorStringLength)
			if toEnum (fromIntegral status) == SqlSuccess
				then do bstr <- peekCString errBuf
					bsts <- peekCString statBuf
					next <- getEnvError env
					return ((showString bstr . showString ", SQLSTATE=" . showString bsts . showChar '\n' . showString next) "")
				else return ""))

---------------------------------------------------

connectToDb :: SqlCon -> (SqlDbcBuf -> IO a) -> IO a
connectToDb con fn = do
	safeAllocEnv (\env -> do
		safeAllocConnect env (\dbu -> do
			safeConnect dbu con (\dbc -> do
				safeAllocBuffer 4096 (\buf -> do
					fn (dbc,buf)))
			`Exception.catch` (\e -> do
				errstr <- getConnectError env dbu
				fail ((shows e . showChar '\n' . showString errstr) "")))
		`Exception.catch` (\e -> do
			errstr <- getEnvError env 
			fail ((shows e . showChar '\n' . showString errstr) "")))

sqlStmt :: SqlDbcBuf -> (SqlStBuf -> IO a) -> IO a
sqlStmt (dbc,buf) stfn = safeAllocStmt dbc (\stmt -> stfn (stmt,buf))


type ColNameList = [String]
type SqlColMap = FiniteMap String String

buildColNameList :: SqlStmt -> SqlCol -> SqlCol -> IO ColNameList
buildColNameList stmt col cols
	| col<=cols = do
		(name,_,_,_,_) <- describeCol stmt col
		nlist <- buildColNameList stmt (col+1) cols
		return (name:nlist)
	| otherwise = return []

getColNameList :: SqlStmt -> IO ColNameList
getColNameList stmt = do
	cols <- numResultCols stmt
	nlist <- buildColNameList stmt 1 cols
	return nlist

buildRowList :: SqlStBuf -> ColNameList -> SqlCol -> IO [(String,String)]
buildRowList _ [] _ = return []
buildRowList stbuf (c:cs) col = do
	(_,v) <- getDataAsString stbuf col
	cvs <- buildRowList stbuf cs (col+1)
	return ((c,v):cvs)
	
getRowAsFM :: SqlStBuf -> ColNameList -> IO SqlColMap
getRowAsFM stbuf clist = do
	rlist <- buildRowList stbuf clist 1
	return (listToFM rlist)

sqlQuery :: SqlStBuf -> String -> IO ColNameList
sqlQuery (stmt,_) q = do
	hPutStr stderr (shows q "\n")
	prepare stmt q
	execute stmt
	getColNameList stmt 
	
