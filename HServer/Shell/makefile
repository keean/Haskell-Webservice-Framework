HFLAGS=-O2 -fvia-C -optc-O2
OBJS=Main.o
SYSLIBS=-syslib net
INCLUDES=-i..
SUBDIRS=
RECURSE=for dir in $(SUBDIRS); do \
	$(MAKE) -C $$dir $@; \
done

.SUFFIXES: .hi .hs .o .c

all: $(OBJS)
	@$(RECURSE)

%.o %.hi:%.hs
	ghc -Wall -c $(HFLAGS) $< $(INCLUDES) $(SYSLIBS)

clean:
	@$(RECURSE)
	rm -f *.o *.hi	Makefile
	cp makefile.in makefile

makefiles:
	@$(RECURSE)
	ghc -M *.hs $(INCLUDES) $(SYSLIBS)
	cat makefile.in Makefile > makefile
	rm -f Makefile

# DO NOT DELETE: Beginning of Haskell dependencies
Main.o : Main.hs
Main.o : ../Lib/HTTP/Types.hi
Main.o : ../Lib/Parser/Parser.hi
Main.o : ../Lib/Linker/Linker.hi
Main.o : ../Lib/Monad/MonadParser.hi
# DO NOT DELETE: End of Haskell dependencies
