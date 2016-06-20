OBJS=Control.o Logic.o Peano.o TypeEq.o HList.o HArray.o HType.o HTypeGHC.o HRecord.o Main.o test
INCLUDES=-i../..

include ../../config/make.conf

test:Main.o HRecord.o HTypeGHC.o HType.o HArray.o HList.o TypeEq.o Peano.o Logic.o Control.o
	ghc -o test Main.o HRecord.o HTypeGHC.o HType.o HArray.o HList.o TypeEq.o Peano.o Logic.o Control.o

