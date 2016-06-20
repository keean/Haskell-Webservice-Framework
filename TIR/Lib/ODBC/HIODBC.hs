{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

-- Haskell IODBC interface
-- Copyright (C) 2002 Keean Schupke

module Lib.ODBC.HIODBC(odbcConnect,connectToDb,safeAllocEnv,safeAllocConnect,safeConnect,safeAllocStmt,safeAllocBuffer,closeCursor,
	prepare,execute,numResultCols,showTables,showColumns,describeCol,fetch,getDataAsString,sqlQuery,getRowAsFM,getRowAsList,
	runSql,getColNameList,SqlAttr,attrAppRowDesc,attrAppParamDesc,attrImpRowDesc,attrImpParamDesc,numParams,ColNameList,getRowsAsListOfLists,
	sqlDo,showRows) where

import GHC.Exception
import Foreign
import GHC.Ptr
import Data.FiniteMap
import Monad
import IO
import System.Posix.Signals

import Lib.Monad.MonadT
import Lib.Monad.MonadState
import Lib.Monad.MonadIO
import Lib.Monad.StateT
import Lib.ODBC.Types

------------------------------------------------------------------------------

safeAllocEnv :: (SqlIO m,SqlIfIO m,MonadPlus m) => (SqlEnv -> m a) -> m a
safeAllocEnv doWith = ioBracket newEnv ioSqlDeleteEnv withEnv where

	newEnv = do
		env <- ioSqlNewEnv
		status <- ioSqlIsValidEnv env
		ioIfFail status (\s -> fail ((showString " Could not malloc in sqlNewEnv (" . shows s) ")")) 
		return env
		
	withEnv env = ioBracket (createEnv env) ioSqlFreeEnv doWith
	
	createEnv env = do
		status <- ioSqlAllocEnv env
		ioIfFail status (\s -> fail ((showString " Bad status returned by sqlAllocEnv (" . shows s) ")"))
		return env

safeAllocConnect :: (SqlIO m,SqlIfIO m,MonadIO m,MonadPlus m) => SqlEnv -> (SqlDbc -> m a) -> m a
safeAllocConnect env doWith = ioBracket ( do
		dbc <- ioSqlNewConnect
		status <- ioSqlIsValidConnect dbc
		ioIfFail status (\s -> fail ((showString " Could not malloc in sqlNewConnect (" . shows s) ")"))
		return dbc) (ioSqlDeleteConnect) (\dbc -> ioBracket ( do
			status <- ioSqlAllocConnect env dbc
			ioIfFail status (\s -> fail((showString " Bad status returned by sqlAllocConnect" . shows s) ")")))
			(\_ -> ioSqlFreeConnect dbc) (\_ -> doWith dbc))

safeConnect :: (SqlIO m,SqlIfIO m,MonadIO m,MonadPlus m) => SqlDbc -> OdbcConnection -> (SqlDbc -> m a) -> m a
safeConnect dbc connection doWith = ioBracket (
		ioBracket (ioNewCStringLen (odbcDsn connection)) (\(dsnS,_) -> ioFree dsnS) (\(dsnS,dsnL) -> 
		ioBracket (ioNewCStringLen (odbcUid connection)) (\(uidS,_) -> ioFree uidS) (\(uidS,uidL) -> 
		ioBracket (ioNewCStringLen (odbcAuth connection)) (\(authS,_) -> ioFree authS) (\(authS,authL) -> do
			status <- ioSqlConnect dbc dsnS (fromIntegral dsnL) uidS (fromIntegral uidL) authS (fromIntegral authL)
			ioIfFail status (\s -> fail ((showString " Bad status returned by sqlConnect (" . shows s) ")"))))))
		(\_ -> ioSqlDisconnect dbc) (\_ -> doWith dbc)

safeAllocStmt :: (SqlIO m,SqlIfIO m,MonadIO m,MonadPlus m) => SqlDbc -> (SqlStmt -> m a) -> m a
safeAllocStmt dbc doWith = ioBracket ( do
		stmt <- ioSqlNewStmt
		status <- ioSqlIsValidStmt stmt
		ioIfFail status (\s -> fail ((showString " Could not malloc in sqlNewStmt (" . shows s) ")"))
		return stmt) (ioSqlDeleteStmt) (\stmt -> ioBracket ( do 
			status <- ioSqlAllocStmt dbc stmt
			ioIfFail status (\s -> fail ((showString " Bad status returned by sqlAllocStmt (" . shows s) ")")))
			(\_ -> ioSqlFreeStmt stmt) (\_ -> doWith stmt))

safeAllocBuffer :: (SqlIO m,SqlIfIO m,MonadIO m,MonadPlus m) => SqlBufferSize -> (SqlBuffer -> m a) -> m a
safeAllocBuffer size doWith = case fromIntegral size of
	sz -> ioBracket ( do
		buf <- ioSqlNewBuffer sz
		status <- ioSqlIsValidBuffer buf
		ioIfFail status (\s -> fail ((showString " Could not malloc in sqlNewBuffer (" . shows s) ")"))
		return buf) (ioSqlDeleteBuffer) (\buf -> do doWith (MkBuffer (buf,sz)))

attrAppRowDesc :: String -> SqlHandle SqlResult
attrAppRowDesc str = ioBracket (ioNewCStringLen str) (\(dS,_) -> ioFree dS) (\(dS,dL) -> do
	state <- getState
	status <- ioSqlSetStmtAttr (sqlStmt state) ((fromIntegral . fromEnum) SqlAttrAppRowDesc) (castPtr dS) (fromIntegral dL)
	ioIfFail status (\s -> fail ((showString " Bad status returned by sqlSetStmtAttr (" . shows s) ")")))
		
attrAppParamDesc :: String -> SqlHandle SqlResult
attrAppParamDesc str = ioBracket (ioNewCStringLen str) (\(dS,_) -> ioFree dS) (\(dS,dL) -> do
	state <- getState
	status <- ioSqlSetStmtAttr (sqlStmt state) ((fromIntegral . fromEnum) SqlAttrAppParamDesc) (castPtr dS) (fromIntegral dL)
	ioIfFail status (\s -> fail ((showString " Bad status returned by sqlSetStmtAttr (" . shows s) ")")))
		
attrImpRowDesc :: String -> SqlHandle SqlResult
attrImpRowDesc str = ioBracket (ioNewCStringLen str) (\(dS,_) -> ioFree dS) (\(dS,dL) -> do
	state <- getState
	status <- ioSqlSetStmtAttr (sqlStmt state) ((fromIntegral . fromEnum) SqlAttrImpRowDesc) (castPtr dS) (fromIntegral dL)
	ioIfFail status (\s -> fail ((showString " Bad status returned by sqlSetStmtAttr (" . shows s) ")")))
		
attrImpParamDesc :: String -> SqlHandle SqlResult
attrImpParamDesc str = ioBracket (ioNewCStringLen str) (\(dS,_) -> ioFree dS) (\(dS,dL) -> do
	state <- getState
	status <- ioSqlSetStmtAttr (sqlStmt state) ((fromIntegral . fromEnum) SqlAttrImpParamDesc) (castPtr dS) (fromIntegral dL)
	ioIfFail status (\s -> fail ((showString " Bad status returned by sqlSetStmtAttr (" . shows s) ")")))

prepare :: SqlColumnName -> SqlHandle SqlResult
prepare cmd = do
	ioBracket (ioNewCStringLen cmd) (\(cmdS,_) -> ioFree cmdS) (\(cmdS,cmdL) -> do
		state <- getState
		status <- ioSqlPrepare (sqlStmt state) cmdS (fromIntegral cmdL)
		ioIfFail status (\s -> fail ((showString " Bad status returned by sqlPrepare (" . shows s) ")")))

numParams :: SqlHandle SqlCol
numParams = ioBracket ( do
	parPtr <- ioSqlNewSmallInt
	status <- ioSqlIsValidSmallInt parPtr
	ioIfFail status (\s -> fail ((showString " Could not malloc parPtr(" . shows s) ")"))
	return parPtr) (ioSqlDeleteSmallInt) (\parPtr -> do
		state <- getState
		status <- ioSqlNumParams (sqlStmt state) parPtr
		ioIfFail status (\s -> fail ((showString " Bad status returned by sqlNumParams (" . shows s) ")"))
		params <- ioSqlGetSmallInt parPtr
		return (fromIntegral params))

execute :: SqlHandle SqlResult
execute = do
	state <- getState
	status <- ioSqlExecute (sqlStmt state)
	ioIfFail status (\s -> fail ((showString " Bad status returned by sqlExecute (" . shows s) ")"))

closeCursor :: SqlHandle SqlResult
closeCursor = do
	state <- getState
	status <- ioSqlCloseCursor (sqlStmt state)
	ioIfFail status (\s -> fail ((showString " Bad status returned by sqlCloseCursor (" . shows s) ")"))

numResultCols :: SqlHandle SqlCol
numResultCols = ioBracket ( do
		colsPtr <- ioSqlNewSmallInt
		status <- ioSqlIsValidSmallInt colsPtr
		ioIfFail status (\s -> fail ((showString " Could not malloc colsPtr (" . shows s) ")"))
		return colsPtr) (ioSqlDeleteSmallInt) (\colsPtr -> do
			state <- getState
			status <- ioSqlNumResultCols (sqlStmt state) colsPtr
			ioIfFail status (\s -> fail ((showString " Bad status returned by sqlNumResultCols (" . shows s) ")"))
			cols <- ioSqlGetSmallInt colsPtr
			return (fromIntegral cols))

describeCol :: SqlCol -> SqlHandle (SqlColumnName,SqlType,SqlColPrecision,SqlColScale,SqlNullable)
describeCol col = ioBracket ( do
	colNamePtr <- ioSqlNewString (fromIntegral maxSqlIdentifierLength)
	status <- ioSqlIsValidString colNamePtr
	ioIfFail status (\s -> fail ((showString " Could not malloc colNamePtr (" . shows s) ")"))
	return colNamePtr) (ioSqlDeleteString) (\colNamePtr -> ioBracket ( do
		colTypePtr <- ioSqlNewSmallInt
		status <- ioSqlIsValidSmallInt colTypePtr
		ioIfFail status (\s -> fail ((showString " Could not malloc colTypePtr (" . shows s) ")"))
		return colTypePtr) (ioSqlDeleteSmallInt) (\colTypePtr -> ioBracket ( do
			colPrecisionPtr <- ioSqlNewUInteger
			status <- ioSqlIsValidUInteger colPrecisionPtr
			ioIfFail status (\s -> fail ((showString " Could not malloc colPrecisionPtr (" . shows s) ")"))
			return colPrecisionPtr) (ioSqlDeleteUInteger) (\colPrecisionPtr -> ioBracket ( do
				colScalePtr <- ioSqlNewSmallInt
				status <- ioSqlIsValidSmallInt colScalePtr
				ioIfFail status (\s -> fail ((showString " Could not malloc colScalePtr (" . shows s) ")"))
				return colScalePtr) (ioSqlDeleteSmallInt) (\colScalePtr -> ioBracket ( do
					colNullablePtr <- ioSqlNewSmallInt
					status <- ioSqlIsValidSmallInt colNullablePtr
					ioIfFail status (\s -> fail ((showString " Could not malloc colNullablePtr" . shows s) ")"))
					return colNullablePtr) (ioSqlDeleteSmallInt) (\colNullablePtr -> do
						state <- getState
						status <- (ioSqlDescribeCol (sqlStmt state) (fromIntegral col) colNamePtr
							(fromIntegral maxSqlIdentifierLength) colTypePtr colPrecisionPtr colScalePtr
							colNullablePtr)
						ioIfFail status (\s -> fail ((showString " Bad status returned by sqlDescribeCol (" . shows s) ")"))
						colName <- ioSqlGetString colNamePtr
						colType <- ioSqlGetSmallInt colTypePtr
						colPrecision <- ioSqlGetUInteger colPrecisionPtr
						colScale <- ioSqlGetSmallInt colScalePtr
						colNullable <- ioSqlGetSmallInt colNullablePtr
						return (colName,toEnum (fromIntegral colType),fromIntegral colPrecision,fromIntegral colScale,toEnum (fromIntegral colNullable)))))))

showTables :: SqlCatalogName -> SqlSchemaName -> SqlTableName -> SqlTableType -> SqlHandle SqlResult
showTables catalogName schemaName tableName tableType = 
	ioBracket (ioNewCStringLen catalogName) (\(catalogNameS,_) -> ioFree catalogNameS) (\(catalogNameS,catalogNameL) -> 
	ioBracket (ioNewCStringLen schemaName) (\(schemaNameS,_) -> ioFree schemaNameS) (\(schemaNameS,schemaNameL) ->
	ioBracket (ioNewCStringLen tableName) (\(tableNameS,_) -> ioFree tableNameS) (\(tableNameS,tableNameL) ->
	ioBracket (ioNewCStringLen tableType) (\(tableTypeS,_) -> ioFree tableTypeS) (\(tableTypeS,tableTypeL) -> do
		state <- getState
		status <- ioSqlTables (sqlStmt state) catalogNameS (fromIntegral catalogNameL)
			schemaNameS (fromIntegral schemaNameL)
			tableNameS (fromIntegral tableNameL)
			tableTypeS (fromIntegral tableTypeL)
		ioIfFail status (\s -> fail ((showString " Bad status returned by sqlTables (" . shows s) ")"))))))

showColumns :: SqlCatalogName -> SqlSchemaName -> SqlTableType -> SqlColumnName -> SqlHandle SqlResult
showColumns catalogName schemaName tableName columnName = 
	ioBracket (ioNewCStringLen catalogName) (\(catalogNameS,_) -> ioFree catalogNameS) (\(catalogNameS,catalogNameL) -> 
	ioBracket (ioNewCStringLen schemaName) (\(schemaNameS,_) -> ioFree schemaNameS) (\(schemaNameS,schemaNameL) ->
	ioBracket (ioNewCStringLen tableName) (\(tableNameS,_) -> ioFree tableNameS) (\(tableNameS,tableNameL) ->
	ioBracket (ioNewCStringLen columnName) (\(columnNameS,_) -> ioFree columnNameS) (\(columnNameS,columnNameL) -> do
		state <- getState
		status <- ioSqlColumns (sqlStmt state) catalogNameS (fromIntegral catalogNameL)
			schemaNameS (fromIntegral schemaNameL)
			tableNameS (fromIntegral tableNameL)
			columnNameS (fromIntegral columnNameL)
		ioIfFail status (\s -> fail ((showString " Bad status returned by sqlColumns (" . shows s) ")"))))))

fetch :: SqlHandle SqlResult
fetch = do
	state <- getState
	status <- ioSqlFetch (sqlStmt state)
	ioIfFail status (\s -> fail ((showString " Bad status returned by sqlFetch (" . shows s) ")"))

getDataAsString :: SqlCol -> SqlHandle (SqlDataAttr,Maybe DataString)
getDataAsString col = ioUnsafeInterleave $ do
	(s,dataAttr) <- ioBracket ( do
		dataAttrPtr <- ioSqlNewInteger
		status <- ioSqlIsValidInteger dataAttrPtr
		ioIfFail status (\s -> fail ((showString " Could not malloc dataAttrPtr (" . shows s) ")"))
		return dataAttrPtr) (ioSqlDeleteInteger) (\dataAttrPtr -> do
			state <- getState
			MkBuffer (bufPtr,bufSz) <- return $ sqlBuffer state
			status <- ioSqlGetData (sqlStmt state) (fromIntegral col) (fromIntegral (fromEnum SqlCharType)) bufPtr bufSz dataAttrPtr
			s <- ioIfFail status (\s -> fail ((showString " Bad status returned by getDataAsString (" . shows s) ")"))
			dataAttr <- ioSqlGetInteger dataAttrPtr
			return (s,dataAttr))
	if dataAttr > 0
		then do
			state <- getState
			if s == SqlSuccessWithInfo
				then do
					MkBuffer (bufPtr,bufSz) <- return (sqlBuffer state)
					dataStr <- ioSqlGetStringLen (castPtr bufPtr,fromIntegral bufSz)
					sd <- getDataAsString col
					case sd of
						(_,Just fdataStr) -> return (fromIntegral dataAttr,Just (dataStr++fdataStr))
						_ -> return (fromIntegral dataAttr,Just dataStr)
				else do
					MkBuffer (bufPtr,_) <- return (sqlBuffer state)
					dataStr <- ioSqlGetStringLen (castPtr bufPtr,fromIntegral dataAttr)
					return (fromIntegral dataAttr,Just dataStr)
		else if dataAttr == 0
			then return (fromIntegral dataAttr,Just "")
			else case (toEnum . fromIntegral) dataAttr of
				SqlDataNull -> return (fromIntegral dataAttr,Nothing)
				_ -> return (fromIntegral dataAttr,Nothing)

getStmtError :: SqlHandle String
getStmtError = ioBracket ( do
	statBuf <- ioSqlNewString (fromIntegral maxSqlErrorStringLength)
	status <- ioSqlIsValidString statBuf
	ioIfFail status (\s -> fail ((showString " Could not malloc status buffer (" . shows s) ")"))
	return statBuf) (ioSqlDeleteString) (\statBuf -> ioBracket ( do
		errBuf <- ioSqlNewString (fromIntegral maxSqlErrorStringLength)
		status <- ioSqlIsValidString errBuf
		ioIfFail status (\s -> fail ((showString " Could not malloc error buffer (" . shows s) ")"))
		return errBuf) (ioSqlDeleteString) (\errBuf -> do
			state <- getState
			status <- ioSqlStmtError (sqlEnv state) (sqlDbc state) (sqlStmt state) statBuf errBuf (fromIntegral maxSqlErrorStringLength)
			if toEnum (fromIntegral status) == SqlSuccess
				then do bstr <- ioSqlGetString errBuf
					bsts <- ioSqlGetString statBuf
					next <- getStmtError
					return ((showString bstr . showString ", SQLSTATE=" . showString bsts . showChar '\n' . showString next) "")
				else return ""))

getConnectError :: SqlEnv -> SqlDbc -> DbHandle String
getConnectError env dbc = ioBracket ( do
	statBuf <- ioSqlNewString (fromIntegral maxSqlErrorStringLength)
	status <- ioSqlIsValidString statBuf
	ioIfFail status (\s -> fail ((showString " Could not malloc status buffer (" . shows s) ")"))
	return statBuf) (ioSqlDeleteString) (\statBuf -> ioBracket ( do
		errBuf <- ioSqlNewString (fromIntegral maxSqlErrorStringLength)
		status <- ioSqlIsValidString errBuf
		ioIfFail status (\s -> fail ((showString " Could not malloc error buffer (" . shows s) ")"))
		return errBuf) (ioSqlDeleteString) (\errBuf -> do
			status <- ioSqlConnectError env dbc statBuf errBuf (fromIntegral maxSqlErrorStringLength)
			if toEnum (fromIntegral status) == SqlSuccess
				then do bstr <- ioSqlGetString errBuf
					bsts <- ioSqlGetString statBuf
					next <- getConnectError env dbc
					return ((showString bstr . showString ", SQLSTATE=" . showString bsts . showChar '\n' . showString next) "")
				else return ""))

getEnvError :: SqlEnv -> DbHandle String
getEnvError env = ioBracket ( do
	statBuf <- ioSqlNewString (fromIntegral maxSqlErrorStringLength)
	status <- ioSqlIsValidString statBuf
	ioIfFail status (\s -> fail ((showString " Could not malloc status buffer (" . shows s) ")"))
	return statBuf) (ioSqlDeleteString) (\statBuf -> ioBracket ( do
		errBuf <- ioSqlNewString (fromIntegral maxSqlErrorStringLength)
		status <- ioSqlIsValidString errBuf
		ioIfFail status (\s -> fail ((showString " Could not malloc error buffer (" . shows s) ")"))
		return errBuf) (ioSqlDeleteString) (\errBuf -> do
			status <- ioSqlEnvError env statBuf errBuf (fromIntegral maxSqlErrorStringLength)
			if toEnum (fromIntegral status) == SqlSuccess
				then do bstr <- ioSqlGetString errBuf
					bsts <- ioSqlGetString statBuf
					next <- getEnvError env
					return ((showString bstr . showString ", SQLSTATE=" . showString bsts . showChar '\n' . showString next) "")
				else return ""))

------------------------------------------------------------------------------

odbcConnect :: (SqlIO m,SqlIfIO m,MonadIO m,MonadPlus m) => OdbcConnection -> StateT SqlState m a -> m a
odbcConnect con m = do
	liftIO (installHandler sigPIPE Ignore Nothing)
	connectToDb con (runSql m)

connectToDb :: (SqlIO m,SqlIfIO m,MonadIO m,MonadPlus m) => OdbcConnection -> (SqlDbcBuf -> m a) -> m a
connectToDb con fn = do
	safeAllocEnv (\env -> do
		safeAllocConnect env (\dbu -> do
			safeConnect dbu con (\dbc -> do
				safeAllocBuffer 4096 (\buf -> do
					fn (env,dbc,buf)))
			`ioCatch` (\e -> do
				errstr <- getConnectError env dbu
				fail ((shows e . showChar '\n' . showString errstr) "")))
		`ioCatch` (\e -> do
			errstr <- getEnvError env 
			fail ((shows e . showChar '\n' . showString errstr) "")))

runSql :: (SqlIO m,SqlIfIO m,MonadIO m,MonadPlus m) => StateT SqlState m a -> SqlDbcBuf -> m a
runSql stfn (env,dbc,buf) = safeAllocStmt dbc (\stmt -> do
	(run (stfn `ioCatch` (\e -> do
		errstr <- getStmtError
		fail ((shows e . showChar '\n' . showString errstr) ""))) $ SqlState env dbc stmt buf))

buildColNameList :: SqlCol -> SqlCol -> SqlHandle ColNameList
buildColNameList col cols
	| col<=cols = do
		(name,_,_,_,_) <- describeCol col
		nlist <- buildColNameList (col+1) cols
		return (name:nlist)
	| otherwise = return []

getColNameList :: SqlHandle ColNameList
getColNameList = do
	cols <- numResultCols
	nlist <- buildColNameList 1 cols
	return nlist

buildRowPairs :: ColNameList -> SqlCol -> SqlHandle [(String,Maybe String)]
buildRowPairs [] _ = return []
buildRowPairs (c:cs) col = do
	(_,v) <- getDataAsString col
	cvs <- buildRowPairs cs (col+1)
	return ((c,v):cvs)
	
getRowAsFM :: ColNameList -> SqlHandle SqlColMap
getRowAsFM clist = do
	rlist <- buildRowPairs clist 1
	return (listToFM rlist)

buildRowList :: SqlCol -> SqlCol -> SqlHandle [Maybe String]
buildRowList col n
	| n>=col = do
		(_,v) <- getDataAsString col
		vs <- buildRowList (col+1) n
		return (v:vs)
	| otherwise = return []

getRowAsList :: Int -> SqlHandle [Maybe String]
getRowAsList n = do
	l <- buildRowList 1 n
	return l

getRowsAsListOfLists :: SqlHandle [[Maybe String]]
getRowsAsListOfLists = do
	cols <- numResultCols
	getRowsAsListOfLists' cols

getRowsAsListOfLists' :: Int -> SqlHandle [[Maybe String]]
getRowsAsListOfLists' cols = ioUnsafeInterleave $ do
	s <- fetch
	case s of
		SqlSuccess -> getRow
		SqlSuccessWithInfo -> getRow
		_ -> return []
	where
	
		getRow :: SqlHandle [[Maybe String]]
		getRow = do
			l0 <- getRowAsList cols
			ls <- getRowsAsListOfLists' cols
			return (l0:ls)

sqlDo :: String -> SqlHandle ()
sqlDo q = do
	ioHPutStr stderr (shows q "\n")
	prepare q
	do { execute; return () } `ioCatch` execError

sqlQuery :: String -> SqlHandle (ColNameList,[[Maybe String]])
sqlQuery q = do
	ioHPutStr stderr (shows q "\n")
	prepare q
	execute `ioCatch` execError
	cols <- getColNameList
	rows <- getRowsAsListOfLists' (length cols)
	return (cols,rows)

execError :: Exception -> SqlHandle a
execError e = do
	errstr <- getStmtError
	fail ((shows e . showChar '\n' . showString errstr) "")
	
showRows :: (MonadIO m,Show r) => [r] -> m ()
showRows (r0:rs) = do
	ioPrint r0
	showRows rs
showRows _ = return ()
		
