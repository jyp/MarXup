{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF marxup3 -F #-}

import MarXup
import MarXup.Latex
import MarXup.Latex.Math
import MarXup.Tex
import MarXup.DerivationTrees
import Control.Applicative
import Data.Monoid
import Control.Monad (unless)
import MarXup.Diagram
import MarXup.Diagram.Graphviz
import Control.Lens (set)
import Data.GraphViz
import Data.String
import Data.GraphViz.Attributes.Complete (Attribute(Shape,Label),Shape(..),Label(StrLabel))

preamble inMP = do
  documentClass "article" []
  usepackage "inputenc" ["utf8x"]
  unless inMP $ usepackage "tikz" []
  usepackage "graphicx" []

arrow :: Object -> Object -> Diagram Incidence
arrow src trg = using (outline "black" . set endTip ToTip) $ do
  edge src trg

autoLab s i = do
  o <- labelObj s
  autoLabel o i

(▸) = flip (#)

grDiag = graph Dot gr

nod x = DotNode x [Shape Circle, Label $ StrLabel $ fromString x]
edg x y = DotEdge x y []
gr :: DotGraph String
gr = DotGraph False True Nothing
     (DotStmts [] []
      [nod "A", nod "B", nod "C", nod "D"]
      [edg "A" "B", edg "A" "C", edg "B" "D"])

testDiagram = do
  -- draw $ path $ circle (Point 0 0) 5
  a   <- labelObj $ math $ "a"
  b   <- labelObj $ math $ "b"
  a'  <- draw $ circleShape -- labelObj $ math $ "c"
  width a' === 15
  b'  <- labelObj $ math $ "d"
  a'' <- labelObj $ math $ "."
  b'' <- labelObj $ math $ "."

  -- c <- texObj $ math $ "c"
  -- Center ▸ c === MP.center [E ▸ a'', E ▸ b''] + (20 +: 0)

  let width = 70
  vdist b a === 30
  hdist a a' === width
  hdist a' a'' === width
  alignMatrix [[Center ▸ a, Center ▸ a',Center ▸ a'']
              ,[Center ▸ b, Center ▸ b',Center ▸ b'']
              ]
  autoLab "bing" =<< arrow a a'
  autoLab "bang" =<< arrow b b'
  autoLab "oops" . swap =<< arrow a b
  autoLab "pif" =<< arrow a' a''
  autoLab "paf" =<< arrow b' b'' 

  draw $ do
    autoLab "equal" =<< edge a'' b''
  return ()

ax c = Rule {delimiter = mempty, ruleStyle = outline "black", ruleLabel = mempty, conclusion = c}

someTree = derivationTreeD  $ Node (rule ("modus ponens") "B")
    [defaultLink ::> Node (ax "X") []
    ,defaultLink  {steps = 0} ::> Node (rule "" "A")
     [defaultLink {steps = 0} ::> Node (rule (braces $ cmd0 "small" <> "mystery") "A1")
       [defaultLink ::> Node (ax "Y") []
       ,defaultLink ::> Node (rule "" "A2")
         [defaultLink ::> Node (rule "" "A3")
        [defaultLink ::> Node (ax "Z") []]]]]
    ,defaultLink {steps = 3} ::> Node (rule "abs" "A --> B")
     [defaultLink ::> Node (rule "" "B")
      [defaultLink ::> Node (ax "A") []
      ]
     ]
    ]

(∶) = binop 2 ", "
γ = Con $ cmd "Gamma" nil
(⊢) = binop 1 (cmd0 "vdash")

x = Con "x"
y = Con "y"
a = Con "a"
b = Con "b"

(≜) = binop 1 "="

main = writeFile ("LaTeX.tex") =<< renderTex preamble «

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

There is also special support for derivation trees:

@section«Derivation Trees»

Here is some derivation tree:

@someTree

@section«Diagrams»

One can also draw diagrams:
@testDiagram


@grDiag
@concl<-section«Conclusion»



Mar@math«@cmd0"chi"»up is awesome.

»

