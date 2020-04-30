EMACS = emacs
LATEXMK = latexmk
LATEXMKFLAGS = -pdf -pdflatex='lualatex -file-line-error -synctex=1'

sources = datatypes.tex

all : datatypes.pdf

datatypes.pdf : ${sources}
	$(LATEXMK) $(LATEXMKFLAGS) datatypes.tex

.PHONY: clean
clean :
	rm -f datatypes.pdf \
              *.log *.bbl *.ind *.ilg *.blg *.bbl *.aux \
              *.toc *.idx *.brf 
