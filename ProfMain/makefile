HFLAGS=-O2 -fvia-C
OBJS=Main.o
SYSLIBS=-syslib data -syslib net 
INCLUDES=-i..
SUBDIRS=
RECURSE=for dir in $(SUBDIRS); do \
	$(MAKE) -C $$dir $@; \
done

.SUFFIXES: .hi .hs .o .c

%.o %.hi:%.hs
	ghc -Wall -c $(HFLAGS) $< $(INCLUDES) $(SYSLIBS)

all: $(OBJS)
	@$(RECURSE)

clean:
	@$(RECURSE)
	rm -f *.o *.hi

# DO NOT DELETE: Beginning of Haskell dependencies
Main.o : Main.hs
Main.o : ../Lib/Server/Types.hi
Main.o : ../Lib/Parser/Parser.hi
Main.o : ../Lib/Linker/Linker.hi
# DO NOT DELETE: End of Haskell dependencies
