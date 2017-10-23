LATEXMK = latexmk
LATEXMKFLAGS = -pdf -pdflatex='lualatex -file-line-error -synctex=1'

sources = semantics.tex

all : semantics.pdf

semantics.pdf : ${sources}
	$(LATEXMK) $(LATEXMKFLAGS) $<

.PHONY: clean
clean :
	rm -f semantics.pdf \
              *.log *.bbl *.ind *.ilg *.blg *.bbl *.aux \
              *.toc *.idx *.brf 
