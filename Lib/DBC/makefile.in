OBJS=Types.o HIODBC_cbits.o HIODBC.o Main.o test HSQL.o 
INCLUDES=-i../../TIR/paper -i../.. 
SYSLIBS=-package containers -package unix -package transformers -package network -package syb

include ../../config/make.conf

test:Main.o HSQL.o HIODBC.o Types.o HIODBC_cbits.o ../Monad/MonadIO.o ../Monad/MonadT.o ../Monad/StateT.o ../Monad/MonadState.o ../Arrow/Runnable.o ../Monad/MonadSoln.o ../Monad/MonadControl.o ../../TIR/paper/Lib/TIR/TIR.o ../../TIR/paper/Lib/TIR/Logic.o ../../TIR/paper/Lib/TIR/Peano.o
	ghc -o test Main.o HSQL.o HIODBC.o Types.o HIODBC_cbits.o ../Monad/MonadIO.o ../Monad/MonadT.o ../Monad/StateT.o ../Monad/MonadState.o ../Arrow/Runnable.o ../Monad/MonadSoln.o ../Monad/MonadControl.o ../../TIR/paper/Lib/TIR/TIR.o ../../TIR/paper/Lib/TIR/Logic.o ../../TIR/paper/Lib/TIR/Peano.o $(SYSLIBS) -lodbc -lodbcinst
