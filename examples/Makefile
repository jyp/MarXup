
mainTest: LaTeX.pdf

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

%.mp %.tex: %.hs
	ghc --make $<
	./$*

%.hspp: %.hs
	ghc -E $<

simpleTest:
	ghc -E Simple.hs
	cat Simple.hspp
	ghc --make Simple.hs
	./Simple
