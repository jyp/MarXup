{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XDoRec -pgmF marxup -F #-}

import MarXup.Latex
import MarXup.Tex
import MarXup.DerivationTrees
import Control.Applicative

preamble :: Tex ()
preamble = do
  usepackage ["mathletters"] "ucs"
  usepackage ["utf8x"] "inputenc"
  usepackage [] "graphicx"


someTree = derivationTree [] $ Node (rule (mbox "modus ponens") "A → B") []

(∶) = binop ":"
γ = cmd "Gamma" nil
(⊢) = binop $ cmd "vdash" nil

x = Tex "x"
y = Tex "y"
a = Tex "a"
b = Tex "b"

(≜) = binop "="

main = render $ latexDocument "article" ["11pt"] preamble $ @"
@intro<-section{Intro}

At-syntax is used to call a Haskell function. The result can be bound.
For example, the @sf{section} command returns a label that can be used
for references.

This is section @xref(intro). Note that cross-references are checked
at ``compile-time''. Forward references also work (see
sec. @xref(concl)).

@section{Markup}

Here comes @sf{some sans-serif text with @em{emphasis}!}

Note that arguments put in braces are markup.


@section{Math}

Arguments in parenthesis are Haskell. Combined with unicode syntax,
this can make writing all sorts of mathy stuff rather pleasant. For
example: @math(γ ⊢ x ∶ a).

The operators are overloaded to work on text as well:
@displayMath(b ≜ sqrt (a + (x/y)))


@concl<-section{Conclusion}



Marχup is awesome.

@"

