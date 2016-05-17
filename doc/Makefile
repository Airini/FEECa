.PHONY: default

NAME    := tech-report
LHS2TEX := lhs2TeX
LATEXMK := latexmk

DEPS := internal.lhs geometry.lhs polynomials.lhs forms.lhs fespaces.lhs

BASE_DIR := ../src/FEECa

INTERNAL_DIR := $(join $(BASE_DIR), /Internal)
UTILITY_DIR  := $(join $(BASE_DIR), /Utility)

INTERNALS := /Vector.lhs
UTILITIES := /Combinatorics.lhs
TOP_LEVEL := /Mask.lhs

PREP := $(join $($(1)), $($(2)))

default : $(NAME).pdf

%.tex : %.lhs %.format $(DEPS)
	$(LHS2TEX) --poly $< > $@

#	$(LHS2TEX) --agda $< > $@

%.pdf : %.tex %.bib
	$(LATEXMK) -pdf $<

#rm $@
#$(LATEXMK) -pdf $<

tech-report.tex :: $(call PREP,INTERNAL_DIR,INTERNALS) $(call PREP,UTILITY_DIR,UTILITIES) $(call PREP,BASE_DIR,TOP_LEVEL)

clean:
	latexmk -C $(NAME).tex
