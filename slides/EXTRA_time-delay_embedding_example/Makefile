FILENAME = slides

all: $(FILENAME)

clean:
	rm -f *~
	rm -f `ls $(FILENAME).* | grep -v .tex | grep -v .pdf`

$(FILENAME):
	echo "Compiling your PhD Thesis...please wait...!"
	rm -f *~
	rm -f `ls $(FILENAME).* | grep -v .tex`
	pdflatex $(FILENAME).tex
	pdflatex $(FILENAME).tex
	pdflatex $(FILENAME).tex
	evince $(FILENAME).pdf &



