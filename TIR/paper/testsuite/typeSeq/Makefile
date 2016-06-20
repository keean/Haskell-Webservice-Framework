ghc = /home/ralf/cvs/software/ghc-fptools/ghc/compiler/stage2/ghc-inplace
#ghc = ghc

all:

ghci:
	${ghc} --interactive Main.hs

ghc:
	${ghc} --make Main.hs

clean:
	rm -f *~
	rm -f *.o
	rm -f *.hi
	rm -f a.out
