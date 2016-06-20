#include <sqlext.h>
#include <sys/types.h>

typedef char unsigned* SQLSTRING_PTR;
typedef char* SQLBUFFER_PTR;
typedef SQLHENV* SQLHENV_PTR;
typedef SQLHDBC* SQLHDBC_PTR;
typedef SQLHSTMT* SQLHSTMT_PTR;
typedef short* SQLSMALLINT_PTR;
typedef unsigned short* SQLUSMALLINT_PTR;
typedef long* SQLINTEGER_PTR;
typedef unsigned long* SQLUINTEGER_PTR;

typedef enum {sql_invalid_handle=-2,sql_error=-1,sql_success=0,sql_success_with_info=1,sql_still_executing=2,sql_need_data=99,sql_no_data_found=100} SqlResult;
typedef enum {sql_unsigned_offset_type=-22,sql_signed_offset_type=-20,sql_bit_type=-7,sql_tiny_int_type=-6,sql_big_int_type=-5,sql_long_varbinary_type=-4,sql_varbinary_type=-3,sql_binary_type=-2,sql_long_varchar_type=-1,sql_unknown_type=0,sql_char_type=1,sql_numeric_type=2,sql_decimal_type=3,sql_integer_type=4,sql_smallint_type=5,sql_float_type=6,sql_real_type=7,sql_double_type=8,sql_date_type=9,sql_time_type=10,sql_timestamp_type=11,sql_varchar_type=12} SqlType;
typedef enum {sql_no_nulls=0,sql_nullable=1,sql_nullable_unknown=2} SqlNullable;
typedef enum {sql_data_null=-1,sql_data_at_exec=-2,sql_nts=-3} SqlDataAttrEnum;

SQLHENV_PTR sqlNewEnv();
SQLHDBC_PTR sqlNewConnect();
SQLHSTMT_PTR sqlNewStmt();
SQLSMALLINT_PTR sqlNewSmallInt();
SQLUSMALLINT_PTR sqlNewUSmallInt();
SQLINTEGER_PTR sqlNewInteger();
SQLUINTEGER_PTR sqlNewUInteger();
SQLSTRING_PTR sqlNewString(SQLSMALLINT);

SQLSMALLINT sqlGetSmallInt(SQLSMALLINT_PTR);
SQLUSMALLINT sqlGetUSmallInt(SQLUSMALLINT_PTR);
SQLINTEGER sqlGetInteger(SQLINTEGER_PTR);
SQLUINTEGER sqlGetUInteger(SQLUINTEGER_PTR);

SQLPOINTER sqlNewBuffer(SQLINTEGER);

SqlResult sqlAllocEnv(SQLHENV_PTR);
SqlResult sqlAllocConnect(SQLHENV_PTR,SQLHDBC_PTR);
SqlResult sqlConnect(SQLHDBC_PTR,SQLSTRING_PTR,SQLSMALLINT,SQLSTRING_PTR,SQLSMALLINT,SQLSTRING_PTR,SQLSMALLINT);
SqlResult sqlAllocStmt(SQLHDBC_PTR,SQLHSTMT_PTR);
SqlResult sqlSetStmtAttr(SQLHDBC_PTR,SQLINTEGER,SQLPOINTER,SQLINTEGER);
SqlResult sqlPrepare(SQLHSTMT_PTR,SQLSTRING_PTR,SQLINTEGER);
SqlResult sqlNumParams(SQLHSTMT_PTR,SQLSMALLINT_PTR);
SqlResult sqlExecute(SQLHSTMT_PTR);
SqlResult sqlCloseCursor(SQLHSTMT_PTR);
SqlResult sqlNumResultCols(SQLHSTMT_PTR,SQLSMALLINT_PTR);
SqlResult sqlDescribeCol(SQLHSTMT_PTR,SQLUSMALLINT,SQLSTRING_PTR,SQLSMALLINT,SQLSMALLINT_PTR,SQLUINTEGER_PTR,SQLSMALLINT_PTR,SQLSMALLINT_PTR);
SqlResult sqlTables(SQLHSTMT_PTR,SQLSTRING_PTR,SQLSMALLINT,SQLSTRING_PTR,SQLSMALLINT,SQLSTRING_PTR,SQLSMALLINT,SQLSTRING_PTR,SQLSMALLINT);
SqlResult sqlColumns(SQLHSTMT_PTR,SQLSTRING_PTR,SQLSMALLINT,SQLSTRING_PTR,SQLSMALLINT,SQLSTRING_PTR,SQLSMALLINT,SQLSTRING_PTR,SQLSMALLINT);
SqlResult sqlFetch(SQLHSTMT_PTR);
SqlResult sqlGetData(SQLHSTMT_PTR,SQLUSMALLINT,SQLSMALLINT,SQLPOINTER,SQLINTEGER,SQLINTEGER_PTR);
SqlResult sqlStmtError(SQLHENV_PTR,SQLHDBC_PTR,SQLHSTMT_PTR,SQLSTRING_PTR,SQLSTRING_PTR,SQLINTEGER);
SqlResult sqlConnectError(SQLHENV_PTR,SQLHDBC_PTR,SQLSTRING_PTR,SQLSTRING_PTR,SQLINTEGER);
SqlResult sqlEnvError(SQLHENV_PTR,SQLSTRING_PTR,SQLSTRING_PTR,SQLINTEGER);
SqlResult sqlFreeStmt(SQLHSTMT_PTR);
SqlResult sqlDisconnect(SQLHDBC_PTR);
SqlResult sqlFreeConnect(SQLHDBC_PTR);
SqlResult sqlFreeEnv(SQLHENV_PTR);

SqlResult sqlIsValidEnv(SQLHENV_PTR);
SqlResult sqlIsValidConnect(SQLHDBC_PTR);
SqlResult sqlIsValidStmt(SQLHSTMT_PTR);
SqlResult sqlIsValidBuffer(SQLPOINTER);
SqlResult sqlIsValidSmallInt(SQLSMALLINT_PTR);
SqlResult sqlIsValidUSmallInt(SQLUSMALLINT_PTR);
SqlResult sqlIsValidInteger(SQLINTEGER_PTR);
SqlResult sqlIsValidUInteger(SQLUINTEGER_PTR);
SqlResult sqlIsValidString(SQLSTRING_PTR);

void sqlDeleteEnv(SQLHENV_PTR);
void sqlDeleteConnect(SQLHDBC_PTR);
void sqlDeleteStmt(SQLHSTMT_PTR);
void sqlDeleteSmallInt(SQLSMALLINT_PTR);
void sqlDeleteUSmallInt(SQLUSMALLINT_PTR);
void sqlDeleteInteger(SQLINTEGER_PTR);
void sqlDeleteUInteger(SQLUINTEGER_PTR);
void sqlDeleteString(SQLSTRING_PTR);
void sqlDeleteBuffer(SQLPOINTER);

