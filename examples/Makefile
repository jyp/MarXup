
all: LaTeX.pdf

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

%.tex: %.hs
	ghc --make $<
	./$*

%.hspp: %.hs
	ghc -E $<

