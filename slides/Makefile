FILENAME = slides

all: $(FILENAME)

LATEX = pdflatex -shell-escape -interaction=nonstopmode -file-line-error
BIB = bibtex



clean:
	rm -f *~
	rm -f `ls $(FILENAME).* | grep -v .tex | grep -v .pdf`

$(FILENAME):
	echo "Compiling your Beamer Presentation...please wait...!"
	rm -f *~
	rm -f `ls $(FILENAME).* | grep -v .tex`

	$(LATEX) $(FILENAME).tex
	$(BIB) $(FILENAME)
	$(LATEX) $(FILENAME).tex
	$(LATEX) $(FILENAME).tex
	evince $(FILENAME).pdf &


	@echo "================================================"
	@echo "All done! report.pdf has been created. - Miguel"
	@echo "================================================"

