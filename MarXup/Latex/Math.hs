{-# LANGUAGE OverloadedStrings #-}
module MarXup.Latex.Math where
import Control.Applicative
import MarXup.Latex
import MarXup.Tex
import MarXup
import Data.Monoid
import Data.Ratio
import Control.Monad (unless)

align  = env "align*" . mkrows . map mkcols

array :: [String] -> String -> [[TeX]] -> TeX
array opts format bod = ensureMath $ do
  env' "array" opts $ do
    braces (tex format)
    mkrows (map mkcols bod)
  return ()

-- | A block
block :: [TeX] -> TeX
block  bod = do
  env "array" $ do
    braces (tex "l")
    mkrows $ bod
  return ()

displayMath,ensureMath,mbox :: Tex a -> Tex a
ensureMath = cmd "ensuremath"
mbox = cmd "mbox"
fbox :: TeX -> TeX
fbox = cmd "fbox"
superscript y = tex "^" <> braces y
subscript x = tex "_" <> braces x

displayMath = env "displaymath"

mathsf :: Tex a -> Tex a
mathsf = cmd "mathsf"

mathpreamble :: ClassFile -> TeX
mathpreamble sty = do
  usepackage "graphicx" []
  usepackage "amsmath"  []
  unless (sty == LNCS) $ usepackage "amsthm"   []
  usepackage "amssymb"  []   -- extra symbols such as □
  usepackage "stmaryrd" [] -- has ⟦ and ⟧
  usepackage "mathpartir" [] -- mathpar environment

  unless (sty == LNCS || sty == Beamer) $ do
    newtheorem "theorem" "Theorem"
    newtheorem "corollary" "Corollary"
    newtheorem "lemma" "Lemma"
    newtheorem "definition" "Definition"
    newtheorem "proposition" "Proposition"

mathpar :: [[TeX]] -> TeX
mathpar = env "mathpar" . mkrows . map mk . filter (not . null)
 where mk = foldr1 (\x y -> x <> cmd0 "and" <> y)

mathbox = mbox . ensureMath

newtheorem :: String -> TeX -> TeX
newtheorem ident txt = cmd "newtheorem" (tex ident) >> braces txt

deflike :: String -> String -> TeX -> TeX -> Tex SortedLabel
deflike reference nv name statement = env'' nv [] [name] $ do
  statement
  label reference

thmlike :: String -> String -> TeX -> TeX -> TeX -> Tex SortedLabel
thmlike reference nv name statement proof = do
  x <- deflike reference nv name statement
  env "proof" proof
  return x

theorem,lemma ::  TeX -> TeX -> TeX -> Tex SortedLabel
theorem = thmlike "Thm." "theorem"
lemma = thmlike "Lem." "lemma"

definition,corollary :: TeX -> TeX -> Tex SortedLabel
definition = deflike "Def." "definition"
corollary = deflike "Cor." "corollary"
proposition = deflike "Prop." "proposition"
example = deflike "Ex." "example"

-- Other stuff
oxford :: Tex a -> Tex a
oxford = bigParenthesize (textual "⟦") (textual "⟧")

multiline' body = env "multline*" $ mkrows body

frac x y = cmdn_ "frac" [x,y]

centerVertically = ensureMath . cmd "vcenter" . cmd "hbox"

qedhere = cmd0 "qedhere"
