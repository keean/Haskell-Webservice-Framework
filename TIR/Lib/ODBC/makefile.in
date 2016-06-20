OBJS=Types.o HIODBC_cbits.o HIODBC.o Main.o test
INCLUDES=-i../.. '-\#include "HIODBC_cbits.h"'

include ../../config/make.conf

test:Main.o HIODBC.o Types.o HIODBC_cbits.o ../Monad/MonadIO.o ../Monad/MonadT.o ../Monad/StateT.o ../Monad/MonadState.o
	ghc -o test Main.o HIODBC.o Types.o HIODBC_cbits.o ../Monad/MonadIO.o ../Monad/MonadT.o ../Monad/StateT.o ../Monad/MonadState.o -lodbc -lodbcinst -syslib net -syslib data
