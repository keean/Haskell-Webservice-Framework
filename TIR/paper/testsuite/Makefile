CWD=${shell pwd}
TOP=${CWD}/../../../ghc-fptools/testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Reference to stage2 compiler supporting interactive use
stage2=${TOP}/../ghc/compiler/stage2/ghc-inplace

# A tweaked main target to use a stage 2 compiler
stage2:
	gmake TEST_HC=${stage2}

# Just do the normal ghc way for testing during development
normal:
	gmake WAY=normal

# Just do the ghci way for testing during development
ghci:
	gmake TEST_HC=${stage2} WAY=ghci

# Should probably be reused from somewhere else
clean:
	find . -depth -name "core"       -exec rm -rf \{\} \;
	find . -depth -name "Main"       -exec rm -rf \{\} \;
	find . -depth -name "*~"         -exec rm -rf \{\} \;
	find . -depth -name "*.o"        -exec rm -rf \{\} \;
	find . -depth -name "*.hi"       -exec rm -rf \{\} \;
	find . -depth -name "*.prof"     -exec rm -rf \{\} \;
	find . -depth -name "*.script"   -exec rm -rf \{\} \;
	find . -depth -name "*.comp.*"   -exec rm -rf \{\} \;
	find . -depth -name "*.interp.*" -exec rm -rf \{\} \;
	find . -depth -name "*.run.*"    -exec rm -rf \{\} \;
	rm -f cast
	rm -f dynamic
	rm -f generic-cast
	rm -f typeSeq/typeSeq

