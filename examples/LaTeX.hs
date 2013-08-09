{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup3 -F #-}

import MarXup
import MarXup.Latex
import MarXup.Latex.Math
import MarXup.Tex
import MarXup.DerivationTrees
import Control.Applicative
import Data.Monoid


preamble inMP = do
  usepackage "inputenc" ["utf8x"] 
  usepackage "graphicx" []


someTree = derivationTreeMP [] $ Node (rule (mbox "modus ponens") "A → B") []

(∶) = binop 2 ", "
γ = Con $ cmd "Gamma" nil
(⊢) = binop 1 (cmd0 "vdash")

x = text "x"
y = text "y"
a = text "a"
b = text "b"

(≜) = binop 1 "="

main = renderToDisk' SVG "test" $ latexDocument preamble $ «

@intro<-section«Intro»

At-syntax is used to call a Haskell function. The result can be bound.
For example, the @sans«section» command returns a label that can be used
for references.

This is section @xref(intro). Note that cross-references are checked
at ``compile-time''. Forward references also work (see
sec. @xref(concl)).

@section«Markup»

Here comes @sans«some sans-serif text with @emph«emphasis»!»

Note that arguments put in braces are markup.

@section«Math»

Arguments in parenthesis are Haskell. Combined with unicode syntax,
this can make writing all sorts  mathy stuff rather pleasant. For
example: @(γ ⊢ x ∶ a).

The operators are overloaded to work on text as well:
@display(b ≜ sqrt (a * (b + (x/y))))

There is also special support for derivation trees (via METAPOST)

@section«Derivation Trees»

Here is some derivation tree:

@someTree

@concl<-section«Conclusion»



Marχup is awesome.

»

