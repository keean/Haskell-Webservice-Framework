t = a4
# t = letter
server=ralf@ssh.cwi.nl
dir=/ufs/ralf/www/HList

all:	paper.dvi 	\
	paper.ps 	\
	paper.pdf
	cd slides; make

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

# The final output is called hmap.ps
paper.ps: paper.dvi
	dvips paper.dvi -o paper.ps -t ${t}

paper.dvi: paper.tex paper.bib Makefile
	latex $*
	bibtex $*
	latex $*
	latex $*

upload:
	cvs commit -m ""
	$(MAKE) all
	cd src; ${MAKE} distr
	@ssh ${server} mkdir -p ${dir}
	@scp -C -p paper.ps  ${server}:${dir}/paper.ps
	@scp -C -p paper.pdf ${server}:${dir}/paper.pdf
	@scp -C -p slides/slides.pdf ${server}:${dir}/slides.pdf
	@scp -C -p slides/slides.ps ${server}:${dir}/slides.ps
	@scp -C -p index.html ${server}:${dir}
	@scp -C -p src/index.html ${server}:${dir}/code.html
	@scp -C -p src/HList.zip ${server}:${dir}
	@scp -C -p src/LICENSE ${server}:${dir}
#	@scp -C -p -r slides-dgp-2004 ${server}:${dir}
	@ssh ${server} chmod -R go+rX ${dir}
