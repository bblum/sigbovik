.PHONY: clean

paper: paper.tex citations.bib
	xelatex $< ; bibtex $(<:%.tex=%) ; xelatex $< ; xelatex $<

clean:
	-rm -f *.aux *.log *.out
