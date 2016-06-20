server=ralf@ssh.cwi.nl
dir=/ufs/ralf/www/OOHaskell

all:	paper.pdf paper.ps

clean:
	rm -f paper.pdf
	rm -f *.toc
	rm -f *.aux
	rm -f *.dvi
	rm -f *.ps
	rm -f *.bbl
	rm -f *.blg
	rm -f *.log
	rm -f *.stackdump
	$(MAKE) purge
	cd src; ${MAKE} clean

makefiles:

purge:
	rm -f *~
	rm -f *.zip

%.pdf: %.ps
	ps2pdf $<

%.pdf : %.fig
	fig2dev -Lpdf -p dummy -b 0 -m0.7 $< >$@
#	epstopdf $*.eps

paper.ps: paper.pdf
	pdf2ps paper.pdf

paper.pdf: paper.tex *.tex paper.bib Makefile
	pdflatex paper
	bibtex paper
	pdflatex paper
	pdflatex paper

upload:
	cvs commit -m ""
	$(MAKE) all
	(cd src; ${MAKE} distr)
	@ssh ${server} mkdir -p ${dir}
	@scp -C -p paper.ps  ${server}:${dir}/paper.ps
	@scp -C -p paper.pdf ${server}:${dir}/paper.pdf
	@scp -C -p index.html ${server}:${dir}
	@scp -C -p src/index.html ${server}:${dir}/code.html
	@scp -C -p src/OOHaskell.zip ${server}:${dir}
	@scp -C -p src/LICENSE ${server}:${dir}
	@ssh ${server} rm -rf ${dir}/src
	@scp -C -p -r src/OOHaskell ${server}:${dir}/src
	cd slides-westpoint; ${MAKE}
	@scp -C -p slides-westpoint/slides.pdf ${server}:${dir}/slides.pdf
	@scp -C -p slides-westpoint/slides.ps ${server}:${dir}/slides.ps
	@ssh ${server} chmod -R go+rX ${dir}
