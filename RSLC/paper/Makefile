.PHONY: all paper proof formal bin clean cleaner
PDFLATEX=${TEXPATH}pdflatex -output-format pdf
BIBTEX=${TEXPATH}bibtex
LATEXTARGET=paper

all: paper

paper:
	${PDFLATEX} ${LATEXTARGET}.tex

bib: paper.bib
	${BIBTEX} paper

clean:
	rm -f *~ *.aux *.log *.bbl *.blg

cleaner: clean
	rm -f *.pdf
