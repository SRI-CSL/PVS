sources = semantics.tex

all : semantics.dvi semantics.ps semantics.ps.gz semantics.pdf

semantics.dvi: $(sources)
	latex $<;
	makeindex -c semantics.idx
	bibtex semantics || echo
	latex $<
	latex $<

semantics.ps.gz : semantics.ps
	gzip -c $< > $@

semantics.ps : semantics.dvi
	dvips -o $@ $<

semantics.pdf : ${sources}
	pdflatex $<

.PHONY: clean
clean :
	rm -f Makefile semantics.dvi semantics.pdf semantics.ps semantics.ps.gz \
              *.log *.bbl *.ind *.ilg *.blg *.bbl *.aux \
              *.toc *.idx *.brf 
