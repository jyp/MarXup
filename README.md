-*- org -*- 

* Example:

file:examples/LaTeX.hs
file:examples/LaTeX.pdf

To test the example:

cabal install
cd examples
ghc --make LaTeX
./LaTeX
pdflatex LaTeX

* Design
Marχup has two parts: a DSL to produce LaTeX code; and a preprocessor
that allows to use markup syntax for that DSL.

** DSL
Syntax: file:MarXup/Tex.hs::Return
Semantics:file:MarXup/Tex.hs::display

** Preprocessor
The preprocessor code is rather ugly :/
Look at the generated code to figure out what it does:

file:examples/LaTeX.hspp

* Further work
file:TODO
The code can be made available on request if
you wish to implement this!

