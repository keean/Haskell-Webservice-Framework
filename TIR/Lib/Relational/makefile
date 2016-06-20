OBJS=Types.o HSQL.o FamDb.o Main.o test
INCLUDES=-i../.. '-\#include "../ODBC/HIODBC_cbits.h"'

include ../../config/make.conf

test:Main.o FamDb.o HSQL.o Types.o ../TIR/HList.o ../TIR/Peano.o ../TIR/Logic.o ../TIR/Control.o ../TIR/HArray.o ../TIR/HType.o ../TIR/HRecord.o ../TIR/HRecordGHC.o ../TIR/HTypeGHC.o ../TIR/TypeEq.o ../ODBC/HIODBC.o ../ODBC/Types.o ../ODBC/HIODBC_cbits.o ../Monad/MonadIO.o ../Monad/MonadT.o ../Monad/StateT.o ../Monad/MonadState.o
	ghc -o test Main.o FamDb.o HSQL.o Types.o ../TIR/HList.o ../TIR/Peano.o ../TIR/Logic.o ../TIR/Control.o ../TIR/HArray.o ../TIR/HType.o ../TIR/HRecord.o ../TIR/HTypeGHC.o ../TIR/HRecordGHC.o ../TIR/TypeEq.o ../ODBC/HIODBC.o ../ODBC/Types.o ../ODBC/HIODBC_cbits.o ../Monad/MonadIO.o ../Monad/MonadT.o ../Monad/StateT.o ../Monad/MonadState.o -lodbc -lodbcinst -syslib net -syslib data
