OBJS=Types.o HSQL.o FamDb.o Main.o test
INCLUDES=-i../.. '-\#include "../DBC/HIODBC_cbits.h"'

include ../../config/make.conf

test:Main.o FamDb.o HSQL.o Types.o ../TIR/HList.o ../TIR/Peano.o ../TIR/Logic.o ../TIR/Control.o ../TIR/HArray.o ../TIR/HType.o ../TIR/HRecord.o ../TIR/HRecordGHC.o ../TIR/HTypeGHC.o ../TIR/TypeEq.o ../DBC/HIODBC.o ../DBC/Types.o ../DBC/HIODBC_cbits.o ../Monad/MonadIO.o ../Monad/MonadT.o ../Monad/StateT.o ../Monad/MonadState.o ../Arrow/Runnable.o ../Monad/MonadSoln.o ../Monad/MonadControl.o
	ghc -o test Main.o FamDb.o HSQL.o Types.o ../TIR/HList.o ../TIR/Peano.o ../TIR/Logic.o ../TIR/Control.o ../TIR/HArray.o ../TIR/HType.o ../TIR/HRecord.o ../TIR/HTypeGHC.o ../TIR/HRecordGHC.o ../TIR/TypeEq.o ../DBC/HIODBC.o ../DBC/Types.o ../DBC/HIODBC_cbits.o ../Monad/MonadIO.o ../Monad/MonadT.o ../Monad/StateT.o ../Monad/MonadState.o ../Arrow/Runnable.o ../Monad/MonadSoln.o ../Monad/MonadControl.o -lodbc -lodbcinst -syslib net -syslib data
