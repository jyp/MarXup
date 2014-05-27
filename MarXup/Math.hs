{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module MarXup.Math where

import Control.Applicative
import MarXup.Latex
import MarXup.Latex.Math
import MarXup.Tex
import MarXup
import Data.Monoid
import Data.Ratio
import Data.String
import Control.Monad (unless)

instance Element Math where
  type Target Math = TeX
  element = inline

instance IsString Math where
  fromString = Con . fromString

inline x = " " <> (cmd "ensuremath" . mRender 0 $ x) <> " "
-- display = cmd "displaymath" . mRender 0
display x = tex "$$" <> mRender 0 x <> tex "$$"

text :: TeX -> Math
text = Con . cmd "text"

-- type MathShallow = Int -> Tex

data Math = BinOp Int (TeX -> TeX -> TeX) Int Int Math Math
          | UnOp Int (TeX -> TeX) Int Math
          | Con TeX
          | Math (Int -> TeX)
          | Invisible (TeX -> TeX) Math

parp p p' = if p' < p then bigParen else id

mRender :: Int -> Math -> TeX
mRender _ (Con x) = x
mRender p (Math x) = x p
mRender p (BinOp p' f pl pr l r) = parp p p' $ f (mRender pl l) (mRender pr r)
mRender p (UnOp p' f px x) = parp p p' $ f (mRender px x)
mRender p (Invisible f x) = f $ mRender p x

ternaryOp :: Int -> (TeX -> TeX -> TeX -> TeX) -> Int -> Int -> Int -> Math -> Math -> Math -> Math
ternaryOp p' f px py pz x y z = Math $ \p -> parp p p' $ f (mRender px x)(mRender py y)(mRender pz z)
  
binop :: Int -> TeX -> Math -> Math -> Math
binop prec op = BinOp prec (\x y -> x <> op <> y) prec prec
preop prec op = UnOp prec (\x -> x <> op) prec
outop left right = UnOp 100 (parenthesize left right) 0
fct x = UnOp 6 (x <>) 7

--------------
-- Operators

(=:) = binop 0 "="

instance Num Math where
  (+) = binop 1 "+"
  (-) = binop 1 "-"
  (*) = binop 2 ""
  abs = outop (cmd0 "mid") (cmd0 "mid")
  signum = preop 10 $ cmd0 "delta"
  fromInteger x = Con $ textual $ show x
  negate = preop 1  "-"

instance Fractional Math where
    (/) = BinOp 10 (\a b -> cmdn_ "frac" [a,b]) 0 0
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Floating Math where
    pi = Con $ cmd0 "pi"
    exp = UnOp 20 (\x -> tex "e^" <> braces x) 0
    sqrt = UnOp 10 (cmd "sqrt") 0
    log = fct (cmd "mathnormal" "log")
    sin = fct (cmd "mathnormal" "sin")
    cos = fct (cmd "mathnormal" "cos")
    asin = fct (cmd "mathnormal" "asin")
    acos = fct (cmd "mathnormal" "acos")
    atan = fct (cmd "mathnormal" "atan")
    sinh = fct (cmd "mathnormal" "sinh")
    cosh = fct (cmd "mathnormal" "cosh")
    asinh = fct (cmd "mathnormal" "asinh")
    acosh = fct (cmd "mathnormal" "acosh")
    atanh = fct (cmd "mathnormal" "atanh")
    (**) = (^^^) 

ceiling, floor :: Math -> Math
ceiling = outop "⌈" "⌉"
floor = outop "⌊" "⌋"




(^^^) = BinOp 5 (\x y -> braces x <> superscript y) 5 6
($$$) = BinOp 5 (\x y -> braces x <> subscript y) 5 6


