OPT_FLAGS= -O2 -optc-O2 -optc-march=native -Wall -i../..
#OPT_FLAGS=
#PROF_FLAGS= -prof -auto-all
PROF_FLAGS=

all:main primelocation MetaSearch.o

ch:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o ch.o 
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -o ch ch.o DOM.o Filter.o Parser.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o -package network -package unix

main:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o MetaSearch.o Main.o 
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -o main Main.o DOM.o Filter.o Parser.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o MetaSearch.o -package network -package unix -package random

primelocation:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o Primelocation.o 
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -o primelocation Primelocation.o DOM.o Filter.o Parser.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o -package network -package unix -package random

jobs:Parser.o DOM.o Filter.o Forest.o Forms.o Jobs.o
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -o jobs Jobs.o DOM.o Filter.o Parser.o Forest.o Forms.o -package network

ch.o:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o ch.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c ch.hs -package network

MetaSearch.o:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o MetaSearch.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c MetaSearch.hs -package network 

Primelocation.o:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o Primelocation.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Primelocation.hs -package network

Main.o:Parser.o DOM.o Filter.o Forest.o Forms.o Cookies.o Redirect.o Shuffle.o Main.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Main.hs -package network 

Jobs.o:Parser.hi DOM.hi Filter.hi Forest.hi Jobs.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Jobs.hs -package network

Parser.o:Parser.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Parser.hs

DOM.o:Parser.hi DOM.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c DOM.hs

Filter.o:Parser.hi DOM.hi Filter.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Filter.hs

Forest.o:Parser.hi DOM.hi Filter.hi Forest.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Forest.hs -package network -package unix

Forms.o:Parser.hi DOM.hi Filter.hi Forms.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Forms.hs

Cookies.o:Parser.hi DOM.hi Filter.hi Cookies.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Cookies.hs 

Redirect.o:Parser.hi DOM.hi Filter.hi Redirect.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Redirect.hs -package network -package unix

source.o:Parser.hi DOM.hi Filter.hi Source.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Source.hs

Shuffle.o:Shuffle.hs
	ghc $(OPT_FLAGS) $(PROF_FLAGS) -c Shuffle.hs

clean:
	rm -f filter *.o *.hi
