HFLAGS=-O2 -fvia-C -optc-O2
OBJS=App.o
SYSLIBS=-syslib data -syslib net
INCLUDES=-i../..
SUBDIRS=
RECURSE=for dir in $(SUBDIRS); do \
	$(MAKE) -C $$dir $@; \
done

.SUFFIXES: .hi .hs .o .c

%.o %.hi:%.hs
	ghc -optl-shared -Wall -c $(HFLAGS) $< $(INCLUDES) $(SYSLIBS) -o $@

all: $(OBJS)
	@$(RECURSE)

clean:
	@$(RECURSE)
	rm -f *.o *.hi Makefile
	cp makefile.in makefile

makefiles:
	@$(RECURSE)
	ghc -M *.hs $(INCLUDES) $(SYSLIBS)
	cat makefile.in Makefile > makefile
	rm -f Makefile
