TEXI2DVI = texi2dvi
DVIPS = dvips
MAKEINFO = makeinfo
TEXI2HTML = texi2html
TEXI2PDF = texi2pdf


all: pvs3.0-release-notes.dvi pvs3.0-release-notes.ps \
     pvs3.0-release-notes.info pvs3.0-release-notes.html \
     pvs3.0-release-notes.pdf

pvs3.0-release-notes.dvi: pvs3.0-release-notes.texi
	$(TEXI2DVI) $<

pvs3.0-release-notes.ps: pvs3.0-release-notes.dvi
	$(DVIPS) -o $@ $<

pvs3.0-release-notes.info: pvs3.0-release-notes.texi
	$(MAKEINFO) $<

pvs3.0-release-notes.html: pvs3.0-release-notes.texi
	$(TEXI2HTML) $<

pvs3.0-release-notes.pdf: pvs3.0-release-notes.texi
	$(TEXI2PDF) $<
