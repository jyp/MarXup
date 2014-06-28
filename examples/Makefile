
all: LaTeX.pdf

%.pdf: %.hs
	ghc --make $<
	./$*
	pdflatex $*
	./$*
	pdflatex $*

%.hspp: %.hs
	ghc -E $<

