all:	paper.dvi 	\
	paper.ps 	\
	paper.pdf

clean:
	rm -f *.aux
	rm -f *.dvi
	rm -f *.ps
	rm -f *.pdf
	rm -f *.bbl
	rm -f *.blg
	rm -f *.log
	$(MAKE) purge
	cd src; ${MAKE} clean

makefiles:

purge:
	rm -f *~
	rm -f *.zip

%.pdf: %.ps
	ps2pdf $<

paper.ps: paper.dvi
	dvips paper.dvi -o paper.ps -t ${t}

paper.dvi: paper.tex paper.bib Makefile
#	latex $*
#	bibtex $*
	latex $*
	latex $*
