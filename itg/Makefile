.PHONY: clean

paper: paper.tex citations.bib
	pdflatex $< ; bibtex $(<:%.tex=%) ; pdflatex $< ; pdflatex $<

clean:
	-rm -f *.aux *.log *.out
