all:
		@$(MAKE) pdf
		@$(MAKE) bib
		@$(MAKE) pdf
		@$(MAKE) pdf
bib:
		bibtex main

pdf:
		pdflatex main
