all: slides.dvi slides.ps slides.pdf

clean:
	rm -f *~
	rm -f *.bak
	rm -f *.dvi
	rm -f *.ps
	rm -f *.pdf
	rm -f *.aux
	rm -f *.log

slides.dvi: slides.tex Makefile
	latex slides

%.pdf: %.dvi
	dvipdf $<

%.ps: %.dvi
	dvips $< -o $@
