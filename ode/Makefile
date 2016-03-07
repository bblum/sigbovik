.PHONY: all ode
PDFLATEX=${TEXPATH}pdflatex -output-format pdf
BIBTEX=${TEXPATH}bibtex
LATEXTARGET=ode

all: ode

ode:
	${PDFLATEX} ${LATEXTARGET}.tex

bib: citations.bib
	${BIBTEX} ode

clean:
	rm -f *~ *.aux *.log *.bbl *.blg

cleaner: clean
	rm -f *.pdf
