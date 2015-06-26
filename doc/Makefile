.PHONY: default

NAME    := tech-report
LHS2TEX := lhs2TeX
LATEXMK := latexmk

DEPS := Geometry.lhs

BASE_DIR := ../src/FEEC

INTERNAL_DIR := $(join $(BASE_DIR), /Internal)
UTILITY_DIR  := $(join $(BASE_DIR), /Utility)

INTERNALS := /Vector.lhs
UTILITIES := /Combinatorics.lhs
TOP_LEVEL :=
# /Mask.lhs

PREP := $(join $($(1)), $($(2)))

default : $(NAME).pdf

%.tex : %.lhs %.format $(DEPS)
	$(LHS2TEX) --poly $< > $@

#	$(LHS2TEX) --agda $< > $@

%.pdf : %.tex %.bib
	$(LATEXMK) -pdf $<

tech-report.tex :: $(call PREP,INTERNAL_DIR,INTERNALS) $(call PREP,UTILITY_DIR,UTILITIES) $(call PREP,BASE_DIR,TOP_LEVEL)

clean:
	latexmk -C $(NAME).tex