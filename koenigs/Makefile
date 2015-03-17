.PHONY: all paper
PDFLATEX=${TEXPATH}xelatex
BIBTEX=${TEXPATH}bibtex
LATEXTARGET=koenigs-pittsburgh

all: paper

paper:
	${PDFLATEX} ${LATEXTARGET}.tex

bib: citations.bib
	${BIBTEX} paper

clean:
	rm -f *~ *.aux *.log *.bbl *.blg

cleaner: clean
	rm -f *.pdf
