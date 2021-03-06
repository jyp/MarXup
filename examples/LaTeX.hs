{-# OPTIONS_GHC -XTypeSynonymInstances -XOverloadedStrings -XRecursiveDo -pgmF dist/build/marxup/marxup -F #-}

import MarXup
import MarXup.Latex
import MarXup.Latex.Math
import MarXup.Math
import MarXup.Tex
import MarXup.LineUp.Haskell
import MarXup.DerivationTrees
import MarXup.PrettyPrint as PP hiding (width)
import MarXup.PrettyPrint.Core as PC
import Control.Applicative
import Data.Monoid
import Control.Monad (unless)
import MarXup.Diagram as D
import Graphics.Diagrams.Plot
import Graphics.Diagrams.Graphviz
import Control.Lens (set)
import Data.GraphViz hiding (Plain)
import Data.String
import Data.Traversable
import Data.GraphViz.Attributes.Complete
  (Attribute(RankSep,Shape,Label,Margin,Width,Len,RankDir),
   Shape(..),Label(StrLabel),DPoint(..),RankDir(..))
import Numeric (showFFloat, showEFloat)

data SExp = Atom String | SX [SExp]

textualS = textual . ($ "")

aPlot :: Dia
aPlot = do
  c@(bx,_) <- simplePlot (Point (textualS . showFFloat (Just 1)) (textualS . showEFloat (Just 0)))
                         (vec (simplLinAxis 0.1,
                               logAxis 10
                               -- simplLinAxis 2000
                              ))
                         (map vec [(0.1,139),(0.35,10035),(0.23,1202)])
  functionPlot c 100 (\x -> 100 + 300000*(x-0.2)^^2)
  width bx === constant 200
  height bx === constant 100
  where vec (x,y) = Point x y

prettyS :: SExp -> Tex Doc
prettyS (Atom x) = PP.text (textual x)
prettyS (SX xs) = do
  xs' <- traverse prettyS xs
  enclosure "(" ")" " " xs'

expr :: TeX
expr = do
    d <- prettyS six
    paragraph "1000"
    PC.pretty 1 1000 d
    paragraph "100"
    PC.pretty 1 200 d
    paragraph "10"
    PC.pretty 1 10 d
  where
  three = SX $ map Atom ["arstarsx","wftwfy","varstw","x","M"]
  six = SX [ three , three , three ]

preamble body = do
  documentClass "article" []
  usepackage "inputenc" ["utf8x"]
  usepackage "graphicx" []
  env "document" body

(▸) = flip (#)

grDiag :: TexDiagram ()
grDiag = graph tex Dot gr

nod x = DotNode x [Margin (DVal 0),Width 0, Shape Circle, Label $ StrLabel $ fromString x]
edg x y z = DotEdge x y [Label $ StrLabel z, Len 0.1]
gr :: DotGraph String
gr = DotGraph False True Nothing
     (DotStmts [GraphAttrs [RankSep [0.1], RankDir FromLeft]] []
      [nod "A", nod "B", nod "C", nod "D"]
      [edg "A" "B" "1"
      ,edg "A" "C" "2"
      ,edg "B" "D" "3"
      ,edg "D" "A" "4"])

testDiagram :: TexDiagram ()
testDiagram = do
  -- draw $ path $ circle (Point 0 0) 5
  a   <- D.label "a" $ ensureMath $ "a"
  b   <- D.label "b" $ ensureMath $ "b"
  a'  <- draw $ circle "a'" -- label $ ensureMath $ "c"
  width a' === constant 15
  b'  <- D.label "b'" $ ensureMath $ "d"
  a'' <- D.label "a''" $ ensureMath $ "."
  b'' <- D.label "b''" $ ensureMath $ "."

  -- c <- texObj $ ensureMath $ "c"
  -- Center ▸ c === MP.center [E ▸ a'', E ▸ b''] + (20 +: 0)

  let width = constant 70
  vdist b a === constant 30
  hdist a a' === width
  hdist a' a'' === width
  alignMatrix [[Center ▸ a, Center ▸ a',Center ▸ a'']
              ,[Center ▸ b, Center ▸ b',Center ▸ b'']
              ]
  autoLab "bing" =<< arrow a a'
  autoLab "bang" =<< arrow b b'
  autoLab "oops" . turn180 =<< arrow a b
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


main = renderTex Plain "LaTeX" docu

docu = preamble «

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

@section«Pretty Printer»
@expr

@section«Derivation Trees»

Here is some derivation tree:

@someTree

@section«Diagrams»
@testDiagram
One can also draw diagrams:

@section«Graphviz»

There is partial, rudimentary support for layout of graphs using graphviz.

@grDiag
%% This is deactivated for now; it requires graphviz to be installed


@section«Plots»

@aPlot

@cmd0"newpage"
@section«Haskell»

There is simple support for lhs2tex-style stuff.

Another paragaph.


@haskell«

autoLab :: String -> OVector -> Diagram TeX Tex Object
autoLab s = autoLabel s (textual s)

»
some text after

@concl<-section«Conclusion»

Mar@ensureMath«@cmd0"chi"»up is awesome :p .

»
