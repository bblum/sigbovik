.PHONY: all paper
PDFLATEX=${TEXPATH}pdflatex -output-format pdf
BIBTEX=${TEXPATH}bibtex
LATEXTARGET=paper

all: paper

paper:
	${PDFLATEX} ${LATEXTARGET}.tex

bib: citations.bib
	${BIBTEX} paper

clean:
	rm -f *~ *.aux *.log *.bbl *.blg

cleaner: clean
	rm -f *.pdf
