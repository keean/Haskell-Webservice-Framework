#include <stdio.h>
#include <stdlib.h>
#include "HIODBC_cbits.h"

#undef DEBUG 

SQLSMALLINT_PTR sqlNewSmallInt() {
	SQLSMALLINT_PTR hint = (SQLSMALLINT_PTR)malloc(sizeof(SQLSMALLINT));
	if (hint!=NULL) {
		*hint=0;
	}
#ifdef DEBUG
	fprintf(stderr,"alloc SmallInt handle = 0x%x\n",(int)hint);
#endif
	return hint;
}

SQLUSMALLINT_PTR sqlNewUSmallInt() {
	SQLUSMALLINT_PTR hint = (SQLUSMALLINT_PTR)malloc(sizeof(SQLUSMALLINT));
	if (hint!=NULL) {
		*hint=0;
	}
#ifdef DEBUG
	fprintf(stderr,"alloc USmallInt handle = 0x%x\n",(int)hint);
#endif
	return hint;
}

SQLINTEGER_PTR sqlNewInteger() {
	SQLINTEGER_PTR hint = (SQLINTEGER_PTR)malloc(sizeof(SQLINTEGER));
	if (hint!=NULL) {
		*hint=0;
	}
#ifdef DEBUG
	fprintf(stderr,"alloc Integer handle = 0x%x\n",(int)hint);
#endif
	return hint;
}

SQLUINTEGER_PTR sqlNewUInteger() {
	SQLUINTEGER_PTR hint = (SQLUINTEGER_PTR)malloc(sizeof(SQLUINTEGER));
	if (hint!=NULL) {
		*hint=0;
	}
#ifdef DEBUG
	fprintf(stderr,"alloc UInteger handle = 0x%x\n",(int)hint);
#endif
	return hint;
}

SQLSTRING_PTR sqlNewString(SQLSMALLINT sz) {
	SQLSTRING_PTR str = (SQLSTRING_PTR)malloc(sizeof(UCHAR)*sz);
	if (str!=NULL) {
		*str='\0';
	}
#ifdef DEBUG
	fprintf(stderr,"alloc String handle = 0x%x\n",(int)str);
#endif
	return str;
}

void sqlDeleteString(SQLSTRING_PTR str) {
#ifdef DEBUG
	fprintf(stderr,"free String handle = 0x%x\n",str);
#endif
	if(str!=NULL) {
		free(str);
	}
}

SQLSMALLINT sqlGetSmallInt(SQLSMALLINT_PTR hint) {
	SQLSMALLINT i=*hint;
#ifdef DEBUG
	fprintf(stderr,"deref SmallInt handle = 0x%x\n",(int)hint);
#endif
	return i;
}

SQLUSMALLINT sqlGetUSmallInt(SQLUSMALLINT_PTR hint) {
	SQLUSMALLINT i=*hint;
#ifdef DEBUG
	fprintf(stderr,"deref USmallInt handle = 0x%x\n",(int)hint);
#endif
	return i;
}

SQLINTEGER sqlGetInteger(SQLINTEGER_PTR hint) {
	SQLINTEGER i=*hint;
#ifdef DEBUG
	fprintf(stderr,"deref Integer handle = 0x%x\n",(int)hint);
#endif
	return i;
}

SQLUINTEGER sqlGetUInteger(SQLUINTEGER_PTR hint) {
	SQLUINTEGER i=*hint;
#ifdef DEBUG
	fprintf(stderr,"deref UInteger handle = 0x%x\n",(int)hint);
#endif
	return i;
}

SQLHENV_PTR sqlNewEnv() {
	SQLHENV_PTR henv = (SQLHENV_PTR)malloc(sizeof(SQLHENV));
	if (henv!=NULL) {
		*henv=SQL_NULL_HENV;
	}
#ifdef DEBUG
	fprintf(stderr,"alloc Env handle = 0x%x\n",(int)henv);
#endif
	return henv;
}

SQLHDBC_PTR sqlNewConnect() {
	SQLHDBC_PTR hdbc = (SQLHDBC_PTR)malloc(sizeof(SQLHDBC));
	if (hdbc!=NULL) {
		*hdbc=SQL_NULL_HDBC;
	}
#ifdef DEBUG
	fprintf(stderr,"alloc Connect handle = 0x%x\n",(int)hdbc);
#endif
	return hdbc;
}

SqlResult sqlIsValidConnect(SQLHDBC_PTR hdbc) {
#ifdef DEBUG
	fprintf(stderr,"test Connect handle = 0x%x\n",(int)hdbc);
#endif
	if (hdbc==NULL || hdbc==SQL_NULL_HDBC) {
		return sql_invalid_handle;
	} else {
		return sql_success;
	}
}

SQLHSTMT_PTR sqlNewStmt() {
	SQLHSTMT_PTR hstmt = (SQLHSTMT_PTR)malloc(sizeof(SQLHSTMT));
	if (hstmt!=NULL) {
		*hstmt=SQL_NULL_HSTMT;
	}
#ifdef DEBUG
	fprintf(stderr,"alloc Statement handle = 0x%x\n",(int)hstmt);
#endif
	return hstmt;
}

SqlResult sqlIsValidStmt(SQLHSTMT_PTR hstmt) {
#ifdef DEBUG
	fprintf(stderr,"test Statement handle = 0x%x\n",(int)hstmt);
#endif
	if (hstmt==NULL || hstmt==SQL_NULL_HSTMT) {
		return sql_invalid_handle;
	} else {
		return sql_success;
	}
}

SQLPOINTER sqlNewBuffer(SQLINTEGER size) {
	SQLPOINTER buf = (SQLPOINTER)malloc(size);
#ifdef DEBUG
	fprintf(stderr,"alloc Buffer handle = 0x%x\n",(int)buf);
#endif
	return buf;
}

SqlResult sqlIsValidBuffer(SQLPOINTER buf) {
#ifdef DEBUG
	fprintf(stderr,"test Buffer handle = 0x%x\n",(int)buf);
#endif
	if (buf==NULL) {
		return sql_invalid_handle;
	} else {
		return sql_success;
	}
}

void sqlDeleteBuffer(SQLPOINTER buf) {
#ifdef DEBUG
	fprintf(stderr,"free Buffer handle = 0x%x\n",(int)buf);
#endif
	if (buf!=NULL) {
		free(buf);
	}
}

SqlResult sqlIsValidEnv(SQLHENV_PTR henv) {
#ifdef DEBUG
	fprintf(stderr,"test Environment handle = 0x%x\n",(int)henv);
#endif
	if (henv==NULL || henv==SQL_NULL_HENV) {
		return sql_invalid_handle;
	} else {
		return sql_success;
	}
}

SqlResult sqlIsValidSmallInt(SQLSMALLINT_PTR ip) {
#ifdef DEBUG
	fprintf(stderr,"test SmallInt handle = 0x%x\n",(int)ip);
#endif
	if (ip==NULL) {
		return sql_invalid_handle;
	} else {
		return sql_success;
	}
}

SqlResult sqlIsValidUSmallInt(SQLUSMALLINT_PTR ip) {
#ifdef DEBUG
	fprintf(stderr,"test USmallInt handle = 0x%x\n",(int)ip);
#endif
	if (ip==NULL) {
		return sql_invalid_handle;
	} else {
		return sql_success;
	}
}

SqlResult sqlIsValidInteger(SQLINTEGER_PTR ip) {
#ifdef DEBUG
	fprintf(stderr,"test Integer handle = 0x%x\n",(int)ip);
#endif
	if (ip==NULL) {
		return sql_invalid_handle;
	} else {
		return sql_success;
	}
}

SqlResult sqlIsValidUInteger(SQLUINTEGER_PTR ip) {
#ifdef DEBUG
	fprintf(stderr,"test UInteger handle = 0x%x\n",(int)ip);
#endif
	if (ip==NULL) {
		return sql_invalid_handle;
	} else {
		return sql_success;
	}
}

SqlResult sqlIsValidString(SQLSTRING_PTR ip) {
#ifdef DEBUG
	fprintf(stderr,"test String handle = 0x%x\n",(int)ip);
#endif
	if (ip==NULL) {
		return sql_invalid_handle;
	} else {
		return sql_success;
	}
}

SqlResult sqlAllocEnv(SQLHENV_PTR henv) {
	SQLRETURN status=SQLAllocEnv(henv);
#ifdef DEBUG
	switch(status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"sqlAllocEnv successful\n");
		break;
	default:
		fprintf(stderr,"sqlAllocEnv failed\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlAllocConnect(SQLHENV_PTR henv,SQLHDBC_PTR hdbc) {
	SQLRETURN status=SQLAllocConnect(*henv,hdbc);
#ifdef DEBUG
	switch(status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"sqlAllocConnect successful\n");
		break;
	default:
		fprintf(stderr,"sqlAllocConnect failed\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlConnect(SQLHDBC_PTR hdbc,SQLSTRING_PTR dsn,SQLSMALLINT dsnL,SQLSTRING_PTR uid,SQLSMALLINT uidL,SQLSTRING_PTR auth,SQLSMALLINT authL) {
	SQLRETURN status = SQLConnect(*hdbc,(SQLCHAR*)dsn,dsnL,(SQLCHAR*)uid,uidL,(SQLCHAR*)auth,authL);
#ifdef DEBUG
	switch (status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"sqlConnect successful\n");
		break;
	default:
		fprintf(stderr,"sqlConnect failed\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlAllocStmt(SQLHDBC_PTR hdbc,SQLHSTMT_PTR hstmt) {
	SQLRETURN status = SQLAllocStmt(*hdbc,hstmt);
#ifdef DEBUG
	switch (status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"sqlAllocStmt successful\n");
		break;
	default:
		fprintf(stderr,"sqlAllocStmt failed\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlSetStmtAttr(SQLHSTMT_PTR hstmt,SQLINTEGER attribute,SQLPOINTER value,SQLINTEGER stringLength) {
	SQLRETURN status=SQLSetStmtAttr(*hstmt,attribute,value,stringLength);
#ifdef DEBUG
	switch (status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"sqlSetStmtAttr successful\n");
		break;
	default:
		fprintf(stderr,"sqlSetStmtAttr failed\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlPrepare(SQLHSTMT_PTR hstmt,SQLSTRING_PTR cmd,SQLINTEGER sz) {
	SQLRETURN status=SQLPrepare(*hstmt,(SQLCHAR*)cmd,sz);
#ifdef DEBUG
	switch (status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"sqlPrepare successful\n");
		break;
	default:
		fprintf(stderr,"sqlPrepare failed\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlNumParams(SQLHSTMT_PTR hstmt,SQLSMALLINT_PTR n) {
	SQLRETURN status=SQLNumParams(*hstmt,n);
#ifdef DEBUG
	switch (status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"sqlNumParams successful\n");
		break;
	default:
		fprintf(stderr,"sqlNumParams failed\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlCloseCursor(SQLHSTMT_PTR hstmt) {
	SQLRETURN status=SQLCloseCursor(*hstmt);
#ifdef DEBUG
	switch (status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"sqlCloseCursor successful\n");
		break;
	default:
		fprintf(stderr,"sqlCloseCursor failed\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlExecute(SQLHSTMT_PTR hstmt) {
	SQLRETURN status=SQLExecute(*hstmt);
#ifdef DEBUG
	switch (status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"sqlExecute successful\n");
		break;
	default:
		fprintf(stderr,"sqlExecute failed\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlStmtError(SQLHENV_PTR henv,SQLHDBC_PTR hdbc,SQLHSTMT_PTR hstmt,SQLSTRING_PTR sqlstate,SQLSTRING_PTR buf,SQLINTEGER bufSz) {
	SQLRETURN status=SQL_ERROR;
#ifdef DEBUG
	fprintf(stderr,"0x%x 0x%x 0x%x 0x%x 0x%x %d\n",henv,hdbc,hstmt,sqlstate,buf,bufSz);
#endif
	status=SQLError(SQL_NULL_HENV,SQL_NULL_HDBC,*hstmt,sqlstate,NULL,buf,(SQLSMALLINT)bufSz,NULL);
#ifdef DEBUG
	fprintf(stderr,"sql error return status=%d\n",status);
#endif
	return (SqlResult)status;
}

SqlResult sqlConnectError(SQLHENV_PTR henv,SQLHDBC_PTR hdbc,SQLSTRING_PTR sqlstate,SQLSTRING_PTR buf,SQLINTEGER bufSz) {
	SQLRETURN status=SQL_ERROR;
#ifdef DEBUG
	fprintf(stderr,"0x%x 0x%x 0x%x 0x%x %d\n",henv,hdbc,sqlstate,buf,bufSz);
#endif
	status=SQLError(SQL_NULL_HENV,*hdbc,SQL_NULL_HSTMT,sqlstate,NULL,buf,(SQLSMALLINT)bufSz,NULL);
#ifdef DEBUG
	fprintf(stderr,"sql error return status=%d\n",status);
#endif
	return (SqlResult)status;
}

SqlResult sqlEnvError(SQLHENV_PTR henv,SQLSTRING_PTR sqlstate,SQLSTRING_PTR buf,SQLINTEGER bufSz) {
	SQLRETURN status=SQL_ERROR;
#ifdef DEBUG
	fprintf(stderr,"0x%x 0x%x 0x%x %d\n",henv,sqlstate,buf,bufSz);
#endif
	status=SQLError(*henv,SQL_NULL_HDBC,SQL_NULL_HSTMT,sqlstate,NULL,buf,(SQLSMALLINT)bufSz,NULL);
#ifdef DEBUG
	fprintf(stderr,"sql error return status=%d\n",status);
#endif
	return (SqlResult)status;
}

SqlResult sqlNumResultCols(SQLHSTMT_PTR hstmt,SQLSMALLINT_PTR cols) {
	SQLRETURN status=SQLNumResultCols(*hstmt,cols);
#ifdef DEBUG
	switch (status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"sqlNumResultCols = %d\n",(int)*cols);
		break;
	default:
		fprintf(stderr,"sqlNumResultCols Failed.\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlDescribeCol(SQLHSTMT_PTR hstmt,SQLUSMALLINT col,SQLSTRING_PTR nameS,SQLSMALLINT nameMaxL,SQLSMALLINT_PTR colType,SQLUINTEGER_PTR colPrecision,SQLSMALLINT_PTR colScale,SQLSMALLINT_PTR colNullable) {
	SQLRETURN status=SQL_ERROR;
#ifdef DEBUG
	fprintf(stderr,"%d %d:",(int)col,(int)nameMaxL);
#endif
	status=SQLDescribeCol(*hstmt,col,nameS,nameMaxL,NULL,colType,colPrecision,colScale,colNullable);
#ifdef DEBUG
	fprintf(stderr,"%s %d %d %d %d\n",nameS,(int)*colType,(int)*colPrecision,(int)*colScale,(int)*colNullable);
#endif
	return (SqlResult)status;
}

SqlResult sqlTables(SQLHSTMT_PTR hstmt,SQLSTRING_PTR catalogNameS,SQLSMALLINT catalogNameL,SQLSTRING_PTR schemaNameS,SQLSMALLINT schemaNameL,SQLSTRING_PTR TableNameS,SQLSMALLINT TableNameL,SQLSTRING_PTR TableTypeS,SQLSMALLINT TableTypeL) {
	SQLRETURN status=SQL_ERROR;
#ifdef DEBUG
	fprintf(stderr,"%s %d %s %d %s %d %s %d\n",catalogNameS,catalogNameL,schemaNameS,schemaNameL,TableNameS,TableNameL,TableTypeS,TableTypeL);
#endif
	status=SQLTables(*hstmt,catalogNameS,catalogNameL,schemaNameS,schemaNameL,TableNameS,TableNameL,TableTypeS,TableTypeL);
	return (SqlResult)status;
}

SqlResult sqlColumns(SQLHSTMT_PTR hstmt,SQLSTRING_PTR catalogNameS,SQLSMALLINT catalogNameL,SQLSTRING_PTR schemaNameS,SQLSMALLINT schemaNameL,SQLSTRING_PTR TableNameS,SQLSMALLINT TableNameL,SQLSTRING_PTR ColumnNameS,SQLSMALLINT ColumnNameL) {
	SQLRETURN status=SQL_ERROR;
#ifdef DEBUG
	fprintf(stderr,"%s %d %s %d %s %d %s %d\n",catalogNameS,catalogNameL,schemaNameS,schemaNameL,TableNameS,TableNameL,ColumnNameS,ColumnNameL);
#endif
	status=SQLColumns(*hstmt,catalogNameS,catalogNameL,schemaNameS,schemaNameL,TableNameS,TableNameL,ColumnNameS,ColumnNameL);
	return (SqlResult)status;
}

SqlResult sqlFetch(SQLHSTMT_PTR hstmt) {
	SQLRETURN status=SQLFetch(*hstmt);
#ifdef DEBUG
	switch(status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"Fetch: has data\n");
		break;
	case SQL_NO_DATA_FOUND:
		fprintf(stderr,"Fetch: no data\n");
		break;
	default:
		fprintf(stderr,"Fetch: failed\n");
		break;
	}
#endif
	return (SqlResult)status;
}

SqlResult sqlGetData(SQLHSTMT_PTR hstmt,SQLUSMALLINT col,SQLSMALLINT type,SQLPOINTER buf,SQLINTEGER bufSz,SQLINTEGER_PTR ind) {
	SQLRETURN status=SQL_ERROR;
#ifdef DEBUG
	fprintf(stderr,"(%d,%d,0x%x,%d)\n",(int)col,(int)type,(int)buf,(int)bufSz);
#endif
	status=SQLGetData(*hstmt,col,type,buf,bufSz,ind);
#ifdef DEBUG
	switch(status) {
	case SQL_SUCCESS:
	case SQL_SUCCESS_WITH_INFO:
		fprintf(stderr,"SQLGetData: Ok, %d\n",ind);
		break;
	case SQL_NO_DATA_FOUND:
		fprintf(stderr,"SQLGetData: no more data...\n");
		break;
	default:
		fprintf(stderr,"SQLGetData: Failed.\n");
		break;
	}
	fprintf(stderr,"%d\n",(int)*ind);
#endif
	return (SqlResult)status;
}

SqlResult sqlFreeStmt(SQLHSTMT_PTR hstmt) {
#ifdef DEBUG
	fprintf(stderr,"free Statement handle = 0x%x\n",hstmt);
#endif
	return (SqlResult)SQLFreeStmt(*hstmt,SQL_DROP);
}

SqlResult sqlDisconnect(SQLHDBC_PTR hdbc) {
#ifdef DEBUG
	fprintf(stderr,"Disconnect handle = 0x%x\n",hdbc);
#endif
	return (SqlResult)SQLDisconnect(*hdbc);
}

SqlResult sqlFreeConnect(SQLHDBC_PTR hdbc) {
#ifdef DEBUG
	fprintf(stderr,"free Connect handle = 0x%x\n",hdbc);
#endif
	return (SqlResult)SQLFreeConnect(*hdbc);
}

SqlResult sqlFreeEnv(SQLHENV_PTR henv) {
#ifdef DEBUG
	fprintf(stderr,"free Env handle = 0x%x\n",henv);
#endif
	return (SqlResult)SQLFreeEnv(*henv);
}

void sqlDeleteEnv(SQLHENV_PTR henv) {
#ifdef DEBUG
	fprintf(stderr,"Delete Env handle = 0x%x\n",henv);
#endif
	if (henv!=NULL) {
		free(henv);
	}
}

void sqlDeleteConnect(SQLHENV_PTR hdbc) {
#ifdef DEBUG
	fprintf(stderr,"Delete Connect handle = 0x%x\n",hdbc);
#endif
	if (hdbc!=NULL) {
		free(hdbc);
	}
}

void sqlDeleteStmt(SQLHENV_PTR hstmt) {
#ifdef DEBUG
	fprintf(stderr,"Delete Statement handle = 0x%x\n",hstmt);
#endif
	if (hstmt!=NULL) {
		free(hstmt);
	}
}

void sqlDeleteSmallInt(SQLSMALLINT_PTR hint) {
#ifdef DEBUG
	fprintf(stderr,"Delete SmallInt handle = 0x%x\n",hint);
#endif
	if (hint!=NULL) {
		free(hint);
	}
}

void sqlDeleteUSmallInt(SQLUSMALLINT_PTR hint) {
#ifdef DEBUG
	fprintf(stderr,"Delete USmallInt handle = 0x%x\n",hint);
#endif
	if (hint!=NULL) {
		free(hint);
	}
}

void sqlDeleteInteger(SQLINTEGER_PTR hint) {
#ifdef DEBUG
	fprintf(stderr,"Delete Integer handle = 0x%x\n",hint);
#endif
	if (hint!=NULL) {
		free(hint);
	}
}

void sqlDeleteUInteger(SQLUINTEGER_PTR hint) {
#ifdef DEBUG
	fprintf(stderr,"Delete UInteger handle = 0x%x\n",hint);
#endif
	if (hint!=NULL) {
		free(hint);
	}
}

