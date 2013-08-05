{-# LANGUAGE OverloadedStrings #-}
module MarXup.Latex.Math where

import Control.Applicative
import MarXup.Latex
import MarXup.Tex
import Data.Monoid
import Data.Ratio

data Math = BinOp Int (TeX -> TeX -> TeX) Int Int Math Math 
          | UnOp Int (TeX -> TeX) Int Math
          | Con TeX
      
parp p p' = if p' < p then bigParen else id

mRender :: Int -> Math -> TeX
mRender _ (Con x) = x
mRender p (BinOp p' f pl pr l r) = parp p p' $ f (mRender pl l) (mRender pr r)
mRender p (UnOp p' f px x) = parp p p' $ f (mRender px x)
 
binop prec op = BinOp prec (\x y -> x <> op <> y) prec prec 
preop prec op = UnOp prec (\x -> x <> op) prec
outop left right = UnOp 100 (parenthesize left right) 0
fct x = UnOp 6 (x <>) 7
 
instance Num Math where
  (+) = binop 1 "+"
  (-) = binop 1 "-"
  (*) = binop 2 "*"
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
    (**) = BinOp 5 (\x y  -> braces x <> tex "^" <> braces y) 5 6

ceiling, floor :: Math -> Math
ceiling = outop "⌈" "⌉"
floor = outop "⌊" "⌋"
