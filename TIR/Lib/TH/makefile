OBJS=TTypeable.o TTypeLift.o TPrelude.o Main.o
INCLUDES=-i.

include ../../config/make.conf

test: TTypeable.o TTypeLift.o TPrelude.o Main.o
	ghc -o test Main.o TPrelude.o TTypeLift.o TTypeable.o -syslib template-haskell

# DO NOT DELETE: Beginning of Haskell dependencies
DH.o : DH.hs
List.o : List.hs
Main.o : Main.hs
Main.o : ./TPrelude.hi
Main.o : ./TTypeable.hi
NamedSem.o : NamedSem.hs
TPrelude.o : TPrelude.hs
TPrelude.o : ./TTypeLift.hi
TPrelude.o : ./TTypeable.hi
TTypeLift.o : TTypeLift.hs
TTypeLift.o : ./TTypeable.hi
TTypeable-new.o : TTypeable-new.hs
TTypeable.o : TTypeable.hs
Template.o : Template.hs
Template.o : DH.hi
Test.o : Test.hs
Test.o : TPrelude.hi
Test.o : TTypeable.hi
tmp.o : tmp.hs
# DO NOT DELETE: End of Haskell dependencies
