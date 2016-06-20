{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

-- Haskell IODBC interface
-- Copyright (C) 2002-2004 Keean Schupke

module Lib.DBC.Types where

import GHC.IOBase (unsafeInterleaveIO)
import Foreign
import Foreign.C
import GHC.Ptr
import Monad
import IO
import Data.FiniteMap
-- import Numeric

import Lib.Monad.MonadT
import Lib.Monad.MonadState
import Lib.Monad.MonadIO
import Lib.Monad.StateT

------------------------------------------------------------------------------
-- Map SQL types to Haskell types.

newtype SqlUnsignedOffset = SqlUnsignedOffset { sqlUnsignedOffset :: Int } deriving Eq
newtype SqlSignedOffset = SqlSignedOffset { sqlSignedOffset :: Int } deriving Eq
newtype SqlTinyInt = SqlTinyInt { sqlTinyInt :: Int } deriving Eq
newtype SqlBigInt = SqlBigInt { sqlBigInt :: Integer } deriving Eq
newtype SqlLongVarbinary = SqlLongVarbinary { sqlLongVarbinary :: Ptr () } deriving Eq
newtype SqlVarbinary = SqlVarbinary { sqlVarBinary :: Ptr () } deriving Eq
newtype SqlBinary = SqlBinary { sqlBinary :: Ptr () } deriving Eq
newtype SqlLongVarchar = SqlLongVarchar { sqlLongVarchar :: String } deriving Eq
data SqlChar = SqlChar { sqlChar :: String, sqlCharLength :: Int } deriving Eq
newtype SqlNumeric = SqlNumeric { sqlNumeric :: Rational } deriving Eq
newtype SqlDecimal = SqlDecimal { sqlDecimal :: Rational } deriving Eq
newtype SqlInteger = SqlInteger { sqlInteger :: Int } deriving Eq
newtype SqlSmallInt = SqlSmallInt { sqlSmallInt :: Int } deriving Eq
newtype SqlFloat = SqlFloat { sqlFloat :: Float } deriving Eq
newtype SqlReal = SqlReal { sqlReal :: Rational } deriving Eq
newtype SqlDouble = SqlDouble { sqlDouble :: Double } deriving Eq
data SqlDate = SqlDate {
   sqlDateYear :: Int,
	sqlDateMonth :: Int,
	sqlDateDay :: Int
} deriving Eq
data SqlTime = SqlTime {
	sqlTimeHour :: Int,
	sqlTimeMinute :: Int,
	sqlTimeSecond :: Int
} deriving Eq
data SqlTimestamp = SqlTimestamp {
   sqlDate :: SqlDate,
   sqlTime :: SqlTime
} deriving Eq
data SqlVarchar = SqlVarchar { sqlVarchar :: String } deriving Eq

class SqlTypeable x where toSqlTypeEnum :: x -> SqlType

instance SqlTypeable SqlUnsignedOffset where toSqlTypeEnum _ = SqlUnsignedOffsetType
instance SqlTypeable SqlSignedOffset where toSqlTypeEnum _ = SqlSignedOffsetType
instance SqlTypeable SqlTinyInt where toSqlTypeEnum _ = SqlTinyIntType
instance SqlTypeable SqlBigInt where toSqlTypeEnum _ = SqlBigIntType
instance SqlTypeable SqlLongVarbinary where toSqlTypeEnum _ = SqlLongVarbinaryType
instance SqlTypeable SqlVarbinary where toSqlTypeEnum _ = SqlVarbinaryType
instance SqlTypeable SqlBinary where toSqlTypeEnum _ = SqlBinaryType
instance SqlTypeable SqlLongVarchar where toSqlTypeEnum _ = SqlLongVarcharType
instance SqlTypeable SqlChar where toSqlTypeEnum _ = SqlCharType
instance SqlTypeable SqlNumeric where toSqlTypeEnum _ = SqlNumericType
instance SqlTypeable SqlDecimal where toSqlTypeEnum _ = SqlDecimalType
instance SqlTypeable SqlInteger where toSqlTypeEnum _ = SqlIntegerType
instance SqlTypeable SqlSmallInt where toSqlTypeEnum _ = SqlSmallIntType
instance SqlTypeable SqlFloat where toSqlTypeEnum _ = SqlFloatType
instance SqlTypeable SqlReal where toSqlTypeEnum _ = SqlRealType
instance SqlTypeable SqlDouble where toSqlTypeEnum _ = SqlDoubleType
instance SqlTypeable SqlDate where toSqlTypeEnum _ = SqlDateType
instance SqlTypeable SqlTime where toSqlTypeEnum _ = SqlTimeType
instance SqlTypeable SqlTimestamp where toSqlTypeEnum _ = SqlTimestampType
instance SqlTypeable SqlVarchar where toSqlTypeEnum _ = SqlVarcharType

------------------------------------------------------------------------------

type SqlEnv = Ptr ()
type SqlDbc = Ptr ()
type SqlStmt = Ptr ()
type SqlReturn = CInt
newtype SqlBuffer = MkBuffer (Ptr (),CLong)

data OdbcConnection = OdbcConnection {
	odbcDsn :: String,
	odbcUid :: String,
	odbcAuth :: String
}

odbcConnection :: OdbcConnection
odbcConnection = OdbcConnection {
	odbcDsn = "",
	odbcUid = "",
	odbcAuth = ""
}

type SqlCatalogName = String
type SqlSchemaName = String
type SqlTableName = String
type SqlTableType = String
type SqlColumnName = String
type SqlCmd = String
type SqlCol = Int
type SqlRow = Int
type SqlNameSz = Int
type SqlColPrecision = Int
type SqlColScale = Int
type SqlBufferSize = Int
type SqlDataAttr = Int
type SqlStBuf = (SqlEnv,SqlDbc,SqlStmt,SqlBuffer)
type SqlDbcBuf = (SqlEnv,SqlDbc,SqlBuffer)
type DataString = String

type ColNameList = [String]
type SqlColMap = FiniteMap String (Maybe String)

data SqlState = SqlState {
	sqlEnv :: SqlEnv,
	sqlDbc :: SqlDbc,
	sqlStmt :: SqlStmt,
	sqlBuffer :: SqlBuffer
}

type SqlHandle a = (SqlIfIO m,SqlIO m,MonadState SqlState m,MonadPlus m) => m a
type DbHandle a = (SqlIO m,SqlIfIO m,MonadIO m,MonadPlus m) => m a

data SqlResult = SqlInvalidHandle
               | SqlError
               | SqlSuccess
               | SqlSuccessWithInfo
               | SqlStillExecuting
               | SqlNeedData
               | SqlNoDataFound
               deriving (Eq,Show)

instance Enum SqlResult where
  fromEnum SqlInvalidHandle = (-2)
  fromEnum SqlError = (-1)
  fromEnum SqlSuccess = 0
  fromEnum SqlSuccessWithInfo = 1
  fromEnum SqlStillExecuting = 2
  fromEnum SqlNeedData = 99
  fromEnum SqlNoDataFound = 100

  toEnum (-2) = SqlInvalidHandle
  toEnum (-1) = SqlError
  toEnum 0 = SqlSuccess
  toEnum 1 = SqlSuccessWithInfo
  toEnum 2 = SqlStillExecuting
  toEnum 99 = SqlNeedData
  toEnum 100 = SqlNoDataFound
  toEnum unmatched = error ("SqlResult.toEnum: Cannot match " ++ show unmatched)

data SqlType = SqlUnsignedOffsetType
             | SqlSignedOffsetType
             | SqlBitType
             | SqlTinyIntType
             | SqlBigIntType
             | SqlLongVarbinaryType
             | SqlVarbinaryType
             | SqlBinaryType
             | SqlLongVarcharType
             | SqlUnknownType
             | SqlCharType
             | SqlNumericType
             | SqlDecimalType
             | SqlIntegerType
             | SqlSmallIntType
             | SqlFloatType
             | SqlRealType
             | SqlDoubleType
             | SqlDateType
             | SqlTimeType
             | SqlTimestampType
             | SqlVarcharType
             deriving Eq

showSqlType :: SqlType -> ShowS
showSqlType SqlUnsignedOffsetType = showString "UNSIGNEDOFFESET"
showSqlType SqlSignedOffsetType = showString "SIGNEDOFFSET"
showSqlType SqlBitType = showString "BIT"
showSqlType SqlTinyIntType = showString "TINYINT"
showSqlType SqlBigIntType = showString "BIGINT"
showSqlType SqlLongVarbinaryType = showString "LONGVARBINARY"
showSqlType SqlVarbinaryType = showString "VARBINARY"
showSqlType SqlBinaryType = showString "BINARY"
showSqlType SqlLongVarcharType = showString "LONGVARCHAR"
showSqlType SqlCharType = showString "CHAR"
showSqlType SqlNumericType = showString "NUMERIC"
showSqlType SqlDecimalType = showString "DECIMAL"
showSqlType SqlIntegerType = showString "INTEGER"
showSqlType SqlSmallIntType = showString "SMALLINT"
showSqlType SqlFloatType = showString "FLOAT"
showSqlType SqlRealType = showString "REAL"
showSqlType SqlDoubleType = showString "DOUBLE"
showSqlType SqlDateType = showString "DATE"
showSqlType SqlTimeType = showString "TIME"
showSqlType SqlTimestampType = showString "TIMESTAMP"
showSqlType SqlVarcharType = showString "VARCHAR"
showSqlType _ = showString "UNKNOWN"

instance Show SqlType where
	showsPrec _ t = showSqlType t

instance Enum SqlType where
  fromEnum SqlUnsignedOffsetType = (-22)
  fromEnum SqlSignedOffsetType = (-20)
  fromEnum SqlBitType = (-7)
  fromEnum SqlTinyIntType = (-6)
  fromEnum SqlBigIntType = (-5)
  fromEnum SqlLongVarbinaryType = (-4)
  fromEnum SqlVarbinaryType = (-3)
  fromEnum SqlBinaryType = (-2)
  fromEnum SqlLongVarcharType = (-1)
  fromEnum SqlUnknownType = 0
  fromEnum SqlCharType = 1
  fromEnum SqlNumericType = 2
  fromEnum SqlDecimalType = 3
  fromEnum SqlIntegerType = 4
  fromEnum SqlSmallIntType = 5
  fromEnum SqlFloatType = 6
  fromEnum SqlRealType = 7
  fromEnum SqlDoubleType = 8
  fromEnum SqlDateType = 9
  fromEnum SqlTimeType = 10
  fromEnum SqlTimestampType = 11
  fromEnum SqlVarcharType = 12

  toEnum (-22) = SqlUnsignedOffsetType
  toEnum (-20) = SqlSignedOffsetType
  toEnum (-7) = SqlBitType
  toEnum (-6) = SqlTinyIntType
  toEnum (-5) = SqlBigIntType
  toEnum (-4) = SqlLongVarbinaryType
  toEnum (-3) = SqlVarbinaryType
  toEnum (-2) = SqlBinaryType
  toEnum (-1) = SqlLongVarcharType
  toEnum 0 = SqlUnknownType
  toEnum 1 = SqlCharType
  toEnum 2 = SqlNumericType
  toEnum 3 = SqlDecimalType
  toEnum 4 = SqlIntegerType
  toEnum 5 = SqlSmallIntType
  toEnum 6 = SqlFloatType
  toEnum 7 = SqlRealType
  toEnum 8 = SqlDoubleType
  toEnum 9 = SqlDateType
  toEnum 10 = SqlTimeType
  toEnum 11 = SqlTimestampType
  toEnum 12 = SqlVarcharType
  toEnum unmatched = error ("SqlType.toEnum: Cannot match " ++ show unmatched)

data SqlNullable = SqlNoNulls
                 | SqlNullable
                 | SqlNullableUnknown
                 deriving (Eq,Show)

instance Enum SqlNullable where
  fromEnum SqlNoNulls = 0
  fromEnum SqlNullable = 1
  fromEnum SqlNullableUnknown = 2

  toEnum 0 = SqlNoNulls
  toEnum 1 = SqlNullable
  toEnum 2 = SqlNullableUnknown
  toEnum unmatched = error ("SqlNullable.toEnum: Cannot match " ++ show unmatched)

data SqlDataAttrEnum = SqlDataNull
                     | SqlDataAtExec
                     | SqlNts
                     deriving (Eq,Show)

instance Enum SqlDataAttrEnum where
  fromEnum SqlDataNull = (-1)
  fromEnum SqlDataAtExec = (-2)
  fromEnum SqlNts = (-3)

  toEnum (-1) = SqlDataNull
  toEnum (-2) = SqlDataAtExec
  toEnum (-3) = SqlNts
  toEnum unmatched = error ("SqlDataAttrEnum.toEnum: Cannot match " ++ show unmatched)

data SqlAttr = SqlAttrAppRowDesc
	| SqlAttrAppParamDesc
	| SqlAttrImpRowDesc
	| SqlAttrImpParamDesc
	| SqlAttrCursorScrollable
	| SqlAttrCursorSensitivity

instance Enum SqlAttr where
	fromEnum SqlAttrAppRowDesc = 10010
	fromEnum SqlAttrAppParamDesc = 10011
	fromEnum SqlAttrImpRowDesc = 10012
	fromEnum SqlAttrImpParamDesc = 10013
	fromEnum SqlAttrCursorScrollable = (-1)
	fromEnum SqlAttrCursorSensitivity = (-2)

	toEnum (-2) = SqlAttrCursorSensitivity
	toEnum (-1) = SqlAttrCursorScrollable
	toEnum (10010) = SqlAttrAppRowDesc
	toEnum (10011) = SqlAttrAppParamDesc
	toEnum (10012) = SqlAttrImpRowDesc
	toEnum (10013) = SqlAttrImpParamDesc
	toEnum unmatched = error ("SqlAttr.toEnum: Cannot match " ++ show unmatched)

data SqlAttrCursorScrollable = SqlNonScrollable | SqlScrollable

instance Enum SqlAttrCursorScrollable where
	fromEnum SqlNonScrollable = 0
	fromEnum SqlScrollable = 1

	toEnum 0 = SqlNonScrollable
	toEnum 1 = SqlScrollable
	toEnum unmatched = error ("SqlAttrCursorScrollable.toEnum: Cannot match " ++ show unmatched)

data SqlAttrCursorSensitivity = SqlUnspecified | SqlInsensitive | SqlSensitive

instance Enum SqlAttrCursorSensitivity where
	fromEnum SqlUnspecified = 0
	fromEnum SqlInsensitive = 1
	fromEnum SqlSensitive = 2

	toEnum 0 = SqlUnspecified
	toEnum 1 = SqlInsensitive
	toEnum 2 = SqlSensitive
	toEnum unmatched = error ("SqlAttrCursorSensitivity.toEnum: Cannot match " ++ show unmatched)

maxSqlIdentifierLength :: Int
maxSqlIdentifierLength = 256 -- ANSI SQL MAX IDENTIFIER LENGTH = 128

maxSqlErrorStringLength :: Int
maxSqlErrorStringLength = 256

------------------------------------------------------------------------------

class Monad m => SqlIfIO m where
	ioIfFail :: SqlReturn -> (SqlResult -> m SqlResult) -> m SqlResult

instance Monad m => SqlIfIO m where
   ioIfFail status fl = case toEnum (fromIntegral status) of
      s  | status < 0 -> fl s
         | otherwise -> return s

instance SqlIfIO m => SqlIfIO (StateT SqlState m) where
   ioIfFail i m = ST $ \s -> do
      a <- ioIfFail i (\a -> run (m a) s)
      return (s,a)


------------------------------------------------------------------------------

class MonadIO m => SqlIO m where
	ioUnsafeInterleave :: m a -> m a
	ioSqlNewString :: CShort -> m (Ptr CChar)
	ioSqlIsValidString :: Ptr CChar -> m SqlReturn
	ioSqlGetString :: Ptr CChar -> m String
	ioSqlGetStringLen :: (Ptr CChar,Int) -> m String
	ioSqlDeleteString :: Ptr CChar -> m ()
	ioSqlNewSmallInt :: m (Ptr CShort)
	ioSqlIsValidSmallInt :: Ptr CShort -> m SqlReturn
	ioSqlGetSmallInt :: Ptr CShort -> m CShort
	ioSqlDeleteSmallInt :: Ptr CShort -> m ()
	ioSqlNewUSmallInt :: m (Ptr CUShort)
	ioSqlIsValidUSmallInt :: Ptr CUShort -> m SqlReturn
	ioSqlGetUSmallInt :: Ptr CUShort -> m CUShort
	ioSqlDeleteUSmallInt :: Ptr CUShort -> m ()
	ioSqlNewInteger :: m (Ptr CLong)
	ioSqlIsValidInteger :: Ptr CLong -> m SqlReturn
	ioSqlGetInteger :: Ptr CLong -> m CLong
	ioSqlDeleteInteger :: Ptr CLong -> m ()
	ioSqlNewUInteger :: m (Ptr CULong)
	ioSqlIsValidUInteger :: Ptr CULong -> m SqlReturn
	ioSqlGetUInteger :: Ptr CULong -> m CULong
	ioSqlDeleteUInteger :: Ptr CULong -> m ()
	ioSqlNewBuffer :: CLong -> m (Ptr ())
	ioSqlIsValidBuffer :: Ptr () -> m SqlReturn
	ioSqlDeleteBuffer :: Ptr () -> m ()
	ioSqlNewEnv :: m SqlEnv
	ioSqlIsValidEnv :: SqlEnv -> m SqlReturn
	ioSqlAllocEnv :: SqlEnv -> m SqlReturn
	ioSqlFreeEnv :: SqlEnv -> m SqlReturn
	ioSqlDeleteEnv :: SqlEnv -> m ()
	ioSqlNewConnect :: m SqlDbc
	ioSqlIsValidConnect :: SqlDbc -> m SqlReturn
	ioSqlAllocConnect :: SqlEnv -> SqlDbc -> m SqlReturn
	ioSqlConnect :: SqlDbc -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> m SqlReturn
	ioSqlDisconnect :: SqlDbc -> m SqlReturn
	ioSqlFreeConnect :: SqlDbc -> m SqlReturn
	ioSqlDeleteConnect :: SqlDbc -> m ()
	ioSqlNewStmt :: m SqlStmt
	ioSqlIsValidStmt :: SqlStmt -> m SqlReturn
	ioSqlAllocStmt :: SqlDbc -> SqlStmt -> m SqlReturn
	ioSqlFreeStmt :: SqlStmt -> m SqlReturn
	ioSqlDeleteStmt :: SqlStmt -> m ()
	ioSqlTables :: SqlStmt -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> m SqlReturn
	ioSqlColumns :: SqlStmt -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> m SqlReturn
	ioSqlPrepare :: SqlStmt -> Ptr CChar -> CLong -> m SqlReturn
	ioSqlNumParams :: SqlStmt -> Ptr CShort -> m SqlReturn
	ioSqlExecute :: SqlStmt -> m SqlReturn
	ioSqlNumResultCols :: SqlStmt -> Ptr CShort -> m SqlReturn
	ioSqlDescribeCol :: SqlStmt -> CUShort -> Ptr CChar -> CShort -> Ptr CShort -> Ptr CULong -> Ptr CShort -> Ptr CShort -> m SqlReturn
	ioSqlFetch :: SqlStmt -> m SqlReturn
	ioSqlGetData :: SqlStmt -> CUShort -> CShort -> Ptr () -> CLong -> Ptr CLong -> m SqlReturn
	ioSqlStmtError :: SqlEnv -> SqlDbc -> SqlStmt -> Ptr CChar -> Ptr CChar -> CLong -> m SqlReturn
	ioSqlConnectError :: SqlEnv -> SqlDbc -> Ptr CChar -> Ptr CChar -> CLong -> m SqlReturn
	ioSqlEnvError :: SqlEnv -> Ptr CChar -> Ptr CChar -> CLong -> m SqlReturn
	ioSqlCloseCursor :: SqlStmt -> m SqlReturn
	ioNewCStringLen :: String -> m (CString,Int)
	ioFree :: CString -> m ()
	ioSqlSetStmtAttr :: SqlStmt -> CLong -> Ptr () -> CLong -> m SqlReturn

instance SqlIO IO where
	ioUnsafeInterleave = unsafeInterleaveIO
	ioSqlNewString = sqlNewString
	ioSqlIsValidString = sqlIsValidString
	ioSqlGetString = peekCString
	ioSqlGetStringLen = peekCStringLen
	ioSqlDeleteString = sqlDeleteString
	ioSqlNewSmallInt = sqlNewSmallInt
	ioSqlIsValidSmallInt = sqlIsValidSmallInt
	ioSqlGetSmallInt = sqlGetSmallInt
	ioSqlDeleteSmallInt = sqlDeleteSmallInt
	ioSqlNewUSmallInt = sqlNewUSmallInt
	ioSqlIsValidUSmallInt = sqlIsValidUSmallInt
	ioSqlGetUSmallInt = sqlGetUSmallInt
	ioSqlDeleteUSmallInt = sqlDeleteUSmallInt
	ioSqlNewInteger = sqlNewInteger
	ioSqlIsValidInteger = sqlIsValidInteger
	ioSqlGetInteger = sqlGetInteger
	ioSqlDeleteInteger = sqlDeleteInteger
	ioSqlNewUInteger = sqlNewUInteger
	ioSqlIsValidUInteger = sqlIsValidUInteger
	ioSqlGetUInteger = sqlGetUInteger
	ioSqlDeleteUInteger = sqlDeleteUInteger
	ioSqlNewBuffer = sqlNewBuffer
	ioSqlIsValidBuffer = sqlIsValidBuffer
	ioSqlDeleteBuffer = sqlDeleteBuffer
	ioSqlNewEnv = sqlNewEnv
	ioSqlIsValidEnv = sqlIsValidEnv
	ioSqlAllocEnv = sqlAllocEnv
	ioSqlFreeEnv = sqlFreeEnv
	ioSqlDeleteEnv = sqlDeleteEnv
	ioSqlNewConnect = sqlNewConnect
	ioSqlIsValidConnect = sqlIsValidConnect
	ioSqlAllocConnect = sqlAllocConnect
	ioSqlConnect = sqlConnect
	ioSqlDisconnect = sqlDisconnect
	ioSqlFreeConnect = sqlFreeConnect
	ioSqlDeleteConnect = sqlDeleteConnect
	ioSqlNewStmt = sqlNewStmt
	ioSqlIsValidStmt = sqlIsValidStmt
	ioSqlAllocStmt = sqlAllocStmt
	ioSqlFreeStmt = sqlFreeStmt
	ioSqlDeleteStmt = sqlDeleteStmt
	ioSqlTables = sqlTables 
	ioSqlColumns = sqlColumns 
	ioSqlPrepare = sqlPrepare
	ioSqlNumParams = sqlNumParams
	ioSqlExecute = sqlExecute
	ioSqlNumResultCols = sqlNumResultCols
	ioSqlDescribeCol = sqlDescribeCol
	ioSqlFetch = sqlFetch
	ioSqlGetData = sqlGetData
	ioSqlStmtError = sqlStmtError
	ioSqlConnectError = sqlConnectError
	ioSqlEnvError = sqlEnvError
	ioSqlCloseCursor = sqlCloseCursor
	ioNewCStringLen = newCStringLen
	ioFree = free
	ioSqlSetStmtAttr = sqlSetStmtAttr

instance (SqlIO m,MonadT t m) => SqlIO (t m) where
	ioUnsafeInterleave = up1 ioUnsafeInterleave
	ioSqlNewString = up . ioSqlNewString
	ioSqlIsValidString = up . ioSqlIsValidString 
	ioSqlGetString = up . ioSqlGetString
	ioSqlGetStringLen = up . ioSqlGetStringLen
	ioSqlDeleteString = up . ioSqlDeleteString
	ioSqlNewSmallInt = up ioSqlNewSmallInt
	ioSqlIsValidSmallInt = up . ioSqlIsValidSmallInt 
	ioSqlGetSmallInt = up . ioSqlGetSmallInt
	ioSqlDeleteSmallInt = up . ioSqlDeleteSmallInt
	ioSqlNewUSmallInt = up ioSqlNewUSmallInt
	ioSqlIsValidUSmallInt = up . ioSqlIsValidUSmallInt 
	ioSqlGetUSmallInt = up . ioSqlGetUSmallInt
	ioSqlDeleteUSmallInt = up . ioSqlDeleteUSmallInt
	ioSqlNewInteger = up ioSqlNewInteger
	ioSqlIsValidInteger = up . ioSqlIsValidInteger 
	ioSqlGetInteger = up . ioSqlGetInteger
	ioSqlDeleteInteger = up . ioSqlDeleteInteger
	ioSqlNewUInteger = up ioSqlNewUInteger
	ioSqlIsValidUInteger = up . ioSqlIsValidUInteger 
	ioSqlGetUInteger = up . ioSqlGetUInteger
	ioSqlNewBuffer = up . ioSqlNewBuffer
	ioSqlIsValidBuffer = up . ioSqlIsValidBuffer
	ioSqlDeleteBuffer = up . ioSqlDeleteBuffer
	ioSqlNewEnv = up ioSqlNewEnv
	ioSqlIsValidEnv = up . ioSqlIsValidEnv
	ioSqlAllocEnv = up . ioSqlAllocEnv
	ioSqlFreeEnv = up . ioSqlFreeEnv
	ioSqlDeleteEnv = up . ioSqlDeleteEnv
	ioSqlNewConnect = up ioSqlNewConnect
	ioSqlIsValidConnect = up . ioSqlIsValidConnect
	ioSqlAllocConnect a = up . ioSqlAllocConnect a
	ioSqlConnect a b c d e f = up . ioSqlConnect a b c d e f
	ioSqlDisconnect = up . ioSqlDisconnect
	ioSqlFreeConnect = up . ioSqlFreeConnect
	ioSqlDeleteConnect = up . ioSqlDeleteConnect
	ioSqlNewStmt = up ioSqlNewStmt
	ioSqlIsValidStmt = up . ioSqlIsValidStmt
	ioSqlAllocStmt a = up . ioSqlAllocStmt a
	ioSqlFreeStmt = up . ioSqlFreeStmt
	ioSqlDeleteStmt = up . ioSqlDeleteStmt
	ioSqlDeleteUInteger = up . ioSqlDeleteUInteger
	ioSqlTables a b c d e f g h = up . ioSqlTables a b c d e f g h
	ioSqlColumns a b c d e f g h = up . ioSqlColumns a b c d e f g h
	ioSqlPrepare s q = up . ioSqlPrepare s q
	ioSqlNumParams s = up . ioSqlNumParams s
	ioSqlExecute = up . ioSqlExecute
	ioSqlNumResultCols s = up . ioSqlNumResultCols s
	ioSqlDescribeCol a b c d e f g = up . ioSqlDescribeCol a b c d e f g
	ioSqlFetch = up . ioSqlFetch 
	ioSqlGetData a b c d e = up . ioSqlGetData a b c d e
	ioSqlStmtError a b c d e = up . ioSqlStmtError a b c d e
	ioSqlConnectError a b c d = up . ioSqlConnectError a b c d
	ioSqlEnvError a b c = up . ioSqlEnvError a b c
	ioSqlCloseCursor = up . ioSqlCloseCursor 
	ioNewCStringLen = up . ioNewCStringLen
	ioFree = up . ioFree
	ioSqlSetStmtAttr a b c = up . ioSqlSetStmtAttr a b c

------------------------------------------------------------------------------

foreign import ccall unsafe "sqlNewEnv"
  sqlNewEnv :: IO SqlEnv

foreign import ccall unsafe "sqlIsValidEnv"
  sqlIsValidEnv :: SqlEnv -> IO SqlReturn

foreign import ccall unsafe "sqlDeleteEnv"
  sqlDeleteEnv :: SqlEnv -> IO ()

foreign import ccall unsafe "sqlAllocEnv"
  sqlAllocEnv :: SqlEnv -> IO SqlReturn

foreign import ccall unsafe "sqlFreeEnv"
  sqlFreeEnv :: SqlEnv -> IO SqlReturn

foreign import ccall unsafe "sqlNewConnect"
  sqlNewConnect :: IO SqlDbc

foreign import ccall unsafe "sqlIsValidConnect"
  sqlIsValidConnect :: SqlDbc -> IO SqlReturn

foreign import ccall unsafe "sqlDeleteConnect"
  sqlDeleteConnect :: SqlDbc -> IO ()

foreign import ccall unsafe "sqlAllocConnect"
  sqlAllocConnect :: SqlEnv -> SqlDbc -> IO SqlReturn

foreign import ccall unsafe "sqlFreeConnect"
  sqlFreeConnect :: SqlDbc -> IO SqlReturn

foreign import ccall unsafe "sqlConnect"
  sqlConnect :: SqlDbc -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> IO SqlReturn

foreign import ccall unsafe "sqlDisconnect"
  sqlDisconnect :: SqlDbc -> IO SqlReturn

foreign import ccall unsafe "sqlNewStmt"
  sqlNewStmt :: IO SqlStmt

foreign import ccall unsafe "sqlIsValidStmt"
  sqlIsValidStmt :: SqlStmt -> IO SqlReturn

foreign import ccall unsafe "sqlDeleteStmt"
  sqlDeleteStmt :: SqlStmt -> IO ()

foreign import ccall unsafe "sqlAllocStmt"
  sqlAllocStmt :: SqlDbc -> SqlStmt -> IO SqlReturn

foreign import ccall unsafe "sqlFreeStmt"
  sqlFreeStmt :: SqlStmt -> IO SqlReturn

foreign import ccall unsafe "sqlNewBuffer"
  sqlNewBuffer :: CLong -> IO (Ptr ())

foreign import ccall unsafe "sqlIsValidBuffer"
  sqlIsValidBuffer :: Ptr () -> IO SqlReturn

foreign import ccall unsafe "sqlDeleteBuffer"
  sqlDeleteBuffer :: Ptr () -> IO ()

foreign import ccall unsafe "sqlSetStmtAttr"
  sqlSetStmtAttr :: SqlStmt -> CLong -> Ptr () -> CLong -> IO SqlReturn

foreign import ccall unsafe "sqlPrepare"
  sqlPrepare :: SqlStmt -> Ptr CChar -> CLong -> IO SqlReturn

foreign import ccall unsafe "sqlNumParams"
  sqlNumParams :: SqlStmt -> Ptr CShort -> IO SqlReturn

foreign import ccall unsafe "sqlCloseCursor"
  sqlCloseCursor :: SqlStmt -> IO SqlReturn

foreign import ccall unsafe "sqlExecute"
  sqlExecute :: SqlStmt -> IO SqlReturn

foreign import ccall unsafe "sqlNewUSmallInt"
  sqlNewUSmallInt :: IO (Ptr CUShort)

foreign import ccall unsafe "sqlIsValidUSmallInt"
  sqlIsValidUSmallInt :: Ptr CUShort -> IO SqlReturn

foreign import ccall unsafe "sqlGetUSmallInt"
  sqlGetUSmallInt :: Ptr CUShort -> IO CUShort

foreign import ccall unsafe "sqlDeleteUSmallInt"
  sqlDeleteUSmallInt :: Ptr CUShort -> IO ()

foreign import ccall unsafe "sqlNewSmallInt"
  sqlNewSmallInt :: IO (Ptr CShort)

foreign import ccall unsafe "sqlIsValidSmallInt"
  sqlIsValidSmallInt :: Ptr CShort -> IO SqlReturn

foreign import ccall unsafe "sqlDeleteSmallInt"
  sqlDeleteSmallInt :: Ptr CShort -> IO ()

foreign import ccall unsafe "sqlNumResultCols"
  sqlNumResultCols :: SqlStmt -> Ptr CShort -> IO SqlReturn

foreign import ccall unsafe "sqlGetSmallInt"
  sqlGetSmallInt :: Ptr CShort -> IO CShort

foreign import ccall unsafe "sqlNewString"
  sqlNewString :: CShort -> IO (Ptr CChar)

foreign import ccall unsafe "sqlIsValidString"
  sqlIsValidString :: Ptr CChar -> IO SqlReturn

foreign import ccall unsafe "sqlDeleteString"
  sqlDeleteString :: Ptr CChar -> IO ()

foreign import ccall unsafe "sqlNewUInteger"
  sqlNewUInteger :: IO (Ptr CULong)

foreign import ccall unsafe "sqlIsValidUInteger"
  sqlIsValidUInteger :: Ptr CULong -> IO SqlReturn

foreign import ccall unsafe "sqlDeleteUInteger"
  sqlDeleteUInteger :: Ptr CULong -> IO ()

foreign import ccall unsafe "sqlTables"
  sqlTables :: SqlStmt -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> IO SqlReturn

foreign import ccall unsafe "sqlColumns"
  sqlColumns :: SqlStmt -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> Ptr CChar -> CShort -> IO SqlReturn

foreign import ccall unsafe "sqlDescribeCol"
  sqlDescribeCol :: SqlStmt -> CUShort -> Ptr CChar -> CShort -> Ptr CShort -> Ptr CULong -> Ptr CShort -> Ptr CShort -> IO SqlReturn

foreign import ccall unsafe "sqlGetUInteger"
  sqlGetUInteger :: Ptr CULong -> IO CULong

foreign import ccall unsafe "sqlFetch"
  sqlFetch :: SqlStmt -> IO SqlReturn

foreign import ccall unsafe "sqlNewInteger"
  sqlNewInteger :: IO (Ptr CLong)

foreign import ccall unsafe "sqlIsValidInteger"
  sqlIsValidInteger :: Ptr CLong -> IO SqlReturn

foreign import ccall unsafe "sqlDeleteInteger"
  sqlDeleteInteger :: Ptr CLong -> IO ()

foreign import ccall unsafe "sqlGetData"
  sqlGetData :: SqlStmt -> CUShort -> CShort -> Ptr () -> CLong -> Ptr CLong -> IO SqlReturn

foreign import ccall unsafe "sqlGetInteger"
  sqlGetInteger :: Ptr CLong -> IO CLong

foreign import ccall unsafe "sqlStmtError"
  sqlStmtError :: SqlEnv -> SqlDbc -> SqlStmt -> Ptr CChar -> Ptr CChar -> CLong -> IO SqlReturn

foreign import ccall unsafe "sqlConnectError"
  sqlConnectError :: SqlEnv -> SqlDbc -> Ptr CChar -> Ptr CChar -> CLong -> IO SqlReturn

foreign import ccall unsafe "sqlEnvError"
  sqlEnvError :: SqlEnv -> Ptr CChar -> Ptr CChar -> CLong -> IO SqlReturn

