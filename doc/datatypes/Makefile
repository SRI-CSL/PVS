sources = datatypes.tex

all : datatypes.dvi datatypes.ps datatypes.ps.gz datatypes.pdf

datatypes.dvi: $(sources)
	latex $<;
	makeindex -c datatypes.idx
	bibtex datatypes || echo
	latex $<
	latex $<

datatypes.ps.gz : datatypes.ps
	gzip -c $< > $@

datatypes.ps : datatypes.dvi
	dvips -o $@ $<

datatypes.pdf : ${sources}
	pdflatex $<

.PHONY: clean
clean :
	rm -f Makefile datatypes.dvi datatypes.pdf datatypes.ps datatypes.ps.gz \
              *.log *.bbl *.ind *.ilg *.blg *.bbl *.aux \
              *.toc *.idx *.brf 
