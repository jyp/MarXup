{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module MarXup.Latex.Math where
import MarXup.Latex
import MarXup.Tex
import MarXup
import Control.Monad (unless)
import MarXup.Verbatim
import Data.String
import Algebra.Classes

type TeXMath = TexMath ()
newtype TexMath a = TexMath {fromTexMath :: Tex a}
  deriving (Functor,Applicative,Monad)

instance Textual TexMath where
  textual = fromString

instance IsString (TexMath ()) where
  fromString = TexMath . tex

instance Semigroup (TexMath ()) where
  TexMath x <> TexMath y = TexMath (x <> y)

instance Monoid (TexMath ()) where
  mempty = mathTex ""

instance Element (TexMath ()) where
  type Target (TexMath ()) = Tex ()
  element = math

escapeMath :: String -> TexMath ()
escapeMath  = TexMath . tex . concatMap escape

mkMathMatrix :: [[TexMath ()]] -> TexMath ()
mkMathMatrix = TexMath . mkrows . map (mkcols . map fromTexMath)

align :: [[TexMath ()]] -> Tex ()
align = env "align*" . fromTexMath . mkMathMatrix

array :: [String] -> String -> [[TexMath ()]] -> TexMath ()
array opts format bod = TexMath $ do
  env' "array" opts $ do
    braces (tex format)
    fromTexMath (mkMathMatrix bod)
  return ()

-- | A block
block :: [TexMath ()] -> TexMath ()
block  bod = TexMath $ do 
  env "array" $ do
    braces (tex "l")
    mkrows $ fmap fromTexMath bod
  return ()

displayMath,math :: TexMath a -> Tex a
math (TexMath bod) = do
  tex "\\("
  x <- bod
  tex "\\)"
  return x
displayMath = env "displaymath" . fromTexMath

mbox,fbox :: Tex a -> TexMath a
mbox = TexMath . cmd "mbox"
fbox = TexMath . cmd "fbox"

text :: Tex a -> TexMath a
text = TexMath . cmd "text"


superscript,subscript :: TexMath () -> TexMath ()
superscript (TexMath y) = TexMath (tex "^" <> braces y)
subscript (TexMath x) = TexMath (tex "_" <> braces x)

mathpreamble :: TeX
mathpreamble = do
  usepkg "amsmath" 50 []
  usepackage "amssymb"  []   -- extra symbols such as □
  usepackage "stmaryrd" [] -- has ⟦ and ⟧


mathpar :: [[TexMath ()]] -> Tex ()
mathpar ps = do
   usepkg "mathpartir" 100 []
   env "mathpar" . mkrows . map mk . filter (not . null) $ (fmap fromTexMath <$> ps)
  where mk = foldr1 (\x y -> x <> cmd0 "and" <> y)

mathbox :: TexMath a -> TexMath a
mathbox = mbox . math


-- | @deflike referent nv header name statement@:
-- Environement of name @nv@, which should be refered as @referent@.
deflike :: String -> String -> String -> TeX -> TeX -> Tex SortedLabel
deflike referent nv header name statement = do
  cls <- askClass
  unless (cls == ACMArt && nv `elem` ["theorem","lemma"]) $ do
    newtheorem nv header
  unless (cls == LNCS) $ usepkg "amsthm" 100 []
  let envir body = case cls of
        SIGPlan -> env'' nv [name] [] (hspace (centimeters 0) >> body)
        ACMArt -> env'' nv [name] [] (body)
        JFP -> env'' nv [name] [] (body)
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
oxford :: TexMath a -> TexMath a
oxford = bigParenthesize (textual "⟦") (textual "⟧")

equation :: TexMath () -> Tex SortedLabel
equation e = do
  env "equation" (fromTexMath e >> label "equation")

multiline' :: [TexMath ()] -> Tex ()
multiline' = env "multline*" . mkrows . fmap fromTexMath

frac :: TexMath () -> TexMath () -> TexMath ()
frac (TexMath x) (TexMath y) = TexMath (cmdn_ "frac" [x,y])

-- centerVertically :: Tex a -> Tex a
-- centerVertically = ensureMath . cmd "vcenter" . cmd "hbox"

qedhere :: Tex ()
qedhere = cmd0 "qedhere"

mathbf, mathtt, mathrm, mathsf, mathnormal :: TexMath a -> TexMath a
mathsf = TexMath . cmd "mathsf" . fromTexMath 
mathnormal = TexMath . cmd "mathnormal" . fromTexMath
mathrm = TexMath . cmd "mathrm" . fromTexMath
mathtt = TexMath . cmd "mathtt" . fromTexMath
mathbf = TexMath . cmd "mathbf" . fromTexMath


paren,brack,brac,bigBrac,bigParen,bigBrack :: TexMath a -> TexMath a
paren = parenthesize (tex "(") (tex ")")
brack = parenthesize (tex "[") (tex "]")
brac = parenthesize (backslash >> tex "{") (backslash >> tex "}")
bigBrac = bigParenthesize (backslash >> tex "{") (backslash >> tex "}")
bigParen = bigParenthesize (tex "(") (tex ")")
bigBrack = bigParenthesize (tex "[") (tex "]")

parenthesize,bigParenthesize :: TeX -> TeX -> TexMath a -> TexMath a
bigParenthesize l r (TexMath bod) = TexMath $ do
  tex "\\left" >> l
  x <- bod
  tex "\\right" >> r
  return x

parenthesize l r (TexMath bod) = TexMath $ do
  l
  x <- bod
  r
  return x

rawMath :: Verbatim a -> TexMath ()
rawMath x = TexMath $ do
  tex $ fromVerbatim x
  return ()

mathTex :: String ->  TexMath ()
mathTex = TexMath . tex

mathFunc :: String -> TexMath ()
mathFunc = TexMath . func

bar :: Char -> TexMath ()
bar c = TexMath (cmd "bar" (tex [c]))

quad :: TexMath ()
quad = TexMath (cmd0 "quad")

stackRel :: TexMath () -> TexMath () -> TexMath ()
stackRel (TexMath op) (TexMath annot) = TexMath (cmdn_ "overset" [annot,op])

leadsTo :: TexMath ()
leadsTo = mathFunc "leadsto"


allowbreak :: TexMath ()
allowbreak = TexMath (cmd0 "allowbreak")

overline :: TexMath a -> TexMath a
overline = TexMath . cmd "overline" . fromTexMath

scriptstyle :: TexMath a -> TexMath a
scriptstyle (TexMath l) = TexMath $ braces $ do
  func "scriptstyle"
  l

instance Additive TeXMath where
  zero = mathTex "0"
  x + y = x <> mathTex "+" <> y
instance Group TeXMath where
  negate x = mathTex "-" <> x
  x - y = x <> mathTex "-" <> y

