TEXI2DVI = texi2dvi
DVIPS = dvips
MAKEINFO = makeinfo
TEXI2HTML = texi2html
TEXI2PDF = texi2pdf


all: pvs-release-notes.dvi pvs-release-notes.ps \
     pvs-release-notes.info pvs-release-notes.html \
     pvs-release-notes.pdf

pvs-release-notes.dvi: pvs-release-notes.texi
	$(TEXI2DVI) $<

pvs-release-notes.ps: pvs-release-notes.dvi
	$(DVIPS) -o $@ $<

pvs-release-notes.info: pvs-release-notes.texi
	$(MAKEINFO) $<

pvs-release-notes.html: pvs-release-notes.texi
	$(TEXI2HTML) $<

pvs-release-notes.pdf: pvs-release-notes.texi
	$(TEXI2PDF) $<
