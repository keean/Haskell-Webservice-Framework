OPT_FLAGS= -O2 -fvia-c -optc-O2 -optc-march=pentium3 -optc-mfpmath=sse -Wall -i../..
#OPT_FLAGS=
#PROF_FLAGS= -prof -auto-all
PROF_FLAGS=

all:main primelocation MetaSearch.o

ch:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o ch.o 
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -o ch ch.o DOM.o Filter.o Parser.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o -syslib net -syslib data -syslib posix

main:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o MetaSearch.o Main.o 
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -o main Main.o DOM.o Filter.o Parser.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o MetaSearch.o -syslib net -syslib data -syslib posix

primelocation:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o Primelocation.o 
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -o primelocation Primelocation.o DOM.o Filter.o Parser.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o -syslib net -syslib data -syslib posix

jobs:Parser.o DOM.o Filter.o Forest.o Forms.o Jobs.o
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -o jobs Jobs.o DOM.o Filter.o Parser.o Forest.o Forms.o -syslib net

ch.o:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o ch.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c ch.hs -syslib net -syslib data

MetaSearch.o:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o MetaSearch.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c MetaSearch.hs -syslib net -syslib data

Primelocation.o:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o Primelocation.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Primelocation.hs -syslib net -syslib data

Main.o:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o Main.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Main.hs -syslib net -syslib data

Jobs.o:Parser.hi DOM.hi Filter.hi Forest.hi Jobs.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Jobs.hs -syslib net

Parser.o:Parser.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Parser.hs

DOM.o:Parser.hi DOM.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c DOM.hs

Filter.o:Parser.hi DOM.hi Filter.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Filter.hs

Forest.o:Parser.hi DOM.hi Filter.hi Forest.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Forest.hs -syslib net -syslib posix

Forms.o:Parser.hi DOM.hi Filter.hi Forms.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Forms.hs

Cookies.o:Parser.hi DOM.hi Filter.hi Cookies.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Cookies.hs -syslib lang

Redirect.o:Parser.hi DOM.hi Filter.hi Redirect.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Redirect.hs -syslib lang -syslib net -syslib posix

source.o:Parser.hi DOM.hi Filter.hi Source.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Source.hs

Shuffle.o:Shuffle.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Shuffle.hs -syslib lang

clean:
	rm -f filter *.o *.hi
