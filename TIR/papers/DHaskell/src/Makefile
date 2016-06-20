all:

test:
	ghci append1.hs < NoTestData.txt
	ghci append2.hs < NoTestData.txt
	ghci append3.hs < NoTestData.txt

clean:
	rm *~

cfg:
	ghci -i./HList cfg.hs
