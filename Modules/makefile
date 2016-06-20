HFLAGS=-O2 -fvia-C -optc-O2
OBJS=
SYSLIBS=
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

makefiles:
	@$(RECURSE)

