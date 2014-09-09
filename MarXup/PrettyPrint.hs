{-# LANGUAGE OverloadedStrings #-}

module MarXup.PrettyPrint where

import Control.Applicative
import Data.Monoid
-- import MarXup.Latex ()
import MarXup.Tex
-- import MarXup.MultiRef (BoxSpec(..))

import MarXup.PrettyPrint.Core
import MarXup.PrettyPrint.Core as MarXup.PrettyPrint(DOC,pretty)

type Docu = Tex DOC

text :: TeX -> Tex DOC
text body = do
  b <- justBox body
  return $ TEXT (body,b)

line :: DOC
line = LINE

nest :: Double -> DOC -> DOC
nest d x = NEST d x

x <<>> y = (<>) <$> x <*> y

bracket l x r = group (l <> nest 0 x <> r)

parens x = do
  bracket <$> text "(" <*> pure x <*> text ")"

x <+> y = x <> space <> y
x </> y = x <> LINE <> y

fill :: [DOC] -> DOC
fill [] = mempty
fill [x] = x
fill (x:y:zs) = (flatten x <+> fill (flatten y : zs)) :<|> (x </> fill (y:zs))

filler :: [Docu] -> Docu
filler ds = fill <$> sequence ds
