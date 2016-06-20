# Some variables
stem     =slides

snippets =	

figs	 = 

texs     = 

all: ${stem}.ps ${stem}.pdf

clean:
	rm -f *.dvi
	rm -f *.bak
	rm -f *.eps
	rm -f *.ps
	rm -f *.pdf
	rm -f *.blg
	rm -f *.bbl
	rm -f *.log
	rm -f *.toc
	rm -f *.aux
	rm -f *~

${stem}.dvi: ${stem}.tex ${snippets} ${texs} ${figs} Makefile
	latex ${stem}

%.pdf: %.dvi
	dvipdf $<

#%.pdf: %.ps
#	ps2pdf $<

%.ps: %.dvi
	dvips $< -o $@
