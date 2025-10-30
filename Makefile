# Makefile for building LaTeX documents

# Main target
.PHONY: paper clean

# Build the paper using latexmk
# -pdf: use pdflatex
# -shell-escape: required for minted package to run Pygments
# -interaction=nonstopmode: don't stop on errors
# draft.tex: the main LaTeX file
paper:
	latexmk -pdf -shell-escape -interaction=nonstopmode draft.tex

# Clean auxiliary files
clean:
	latexmk -C draft.tex
	rm -rf _minted-draft
	rm -f *.bbl *.run.xml
