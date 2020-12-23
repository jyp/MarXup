{-# LANGUAGE OverloadedStrings #-}
module MarXup.Latex.Math where
import Control.Applicative
import MarXup.Latex
import MarXup.Tex
import MarXup
import Data.Monoid
import Control.Monad (unless)

align :: [[TeX]] -> Tex ()
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
superscript :: Tex () -> TeX
superscript y = tex "^" <> braces y
subscript :: Tex () -> TeX
subscript x = tex "_" <> braces x

displayMath = env "displaymath"

mathpreamble :: TeX
mathpreamble = do
  usepackage "amsmath"  []
  usepackage "amssymb"  []   -- extra symbols such as □
  usepackage "stmaryrd" [] -- has ⟦ and ⟧


mathpar :: [[TeX]] -> TeX
mathpar ps = do
   usepkg "mathpartir" 100 []
   env "mathpar" . mkrows . map mk . filter (not . null) $ ps
  where mk = foldr1 (\x y -> x <> cmd0 "and" <> y)

mathbox :: Tex a -> Tex a
mathbox = mbox . ensureMath


-- | @deflike referent nv header name statement@:
-- Environement of name @nv@, which should be refered as @referent@.
deflike :: String -> String -> String -> TeX -> TeX -> Tex SortedLabel
deflike referent nv header name statement = do
  cls <- askClass
  unless (cls == ACMArt && nv `elem` ["theorem","lemma"]) $ do
    newtheorem nv header
  unless (cls == LNCS) $ usepkg "amsthm" 100 []
  let envir body = case cls of
        SIGPlan -> env'' nv [name] [] (hspace "0cm" >> body)
        _ -> env'' nv [] [name] body
  envir $ do
    statement
    label referent

thmlike :: String -> String -> String -> TeX -> TeX -> TeX -> Tex SortedLabel
thmlike referent nv header name statement proof = do
  x <- deflike referent nv header name statement
  env "proof" proof
  return x

theorem,lemma ::  TeX -> TeX -> TeX -> Tex SortedLabel
theorem = thmlike "Thm." "theorem" "Theorem"
lemma = thmlike "Lem." "lemma" "Lemma"

definition,corollary,proposition,example :: TeX -> TeX -> Tex SortedLabel
definition = deflike "Def." "definition" "Definition"
corollary = deflike "Cor." "corollary" "Corollary"
proposition = deflike "Prop." "proposition" "Proposition"
example = deflike "Ex." "example" "Example"

-- Other stuff
oxford :: Tex a -> Tex a
oxford = bigParenthesize (textual "⟦") (textual "⟧")

multiline' :: [TeX] -> Tex ()
multiline' body = env "multline*" $ mkrows body

frac :: TeX -> TeX -> Tex ()
frac x y = cmdn_ "frac" [x,y]

centerVertically :: Tex a -> Tex a
centerVertically = ensureMath . cmd "vcenter" . cmd "hbox"

qedhere :: Tex ()
qedhere = cmd0 "qedhere"


mathnormal :: Tex a -> Tex a
mathnormal = cmd "mathnormal"

mathsf :: Tex a -> Tex a
mathsf = cmd "mathsf" 

mathrm :: Tex a -> Tex a
mathrm = cmd "mathrm"

mathtt :: Tex a -> Tex a
mathtt = cmd "mathtt"

mathbf :: Tex a -> Tex a
mathbf = cmd "mathbf"
