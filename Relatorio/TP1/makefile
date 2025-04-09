TEXFILE=RelatorioTP1

LATEX=pdflatex
BIBTEX=bibtex
FLAGS=-interaction=nonstopmode -halt-on-error

all: $(TEXFILE).pdf

$(TEXFILE).pdf: $(TEXFILE).tex
	$(LATEX) $(FLAGS) $(TEXFILE).tex
	$(BIBTEX) $(TEXFILE) || true
	$(LATEX) $(FLAGS) $(TEXFILE).tex
	$(LATEX) $(FLAGS) $(TEXFILE).tex

clean:
	rm -f *.aux *.log *.bbl *.blg *.out *.toc *.lof *.lot *.fls *.fdb_latexmk $(TEXFILE).pdf

distclean: clean
	rm -f $(TEXFILE).pdf
