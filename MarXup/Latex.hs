{-# LANGUAGE OverloadedStrings #-}
module MarXup.Latex where

import Control.Applicative
import MarXup.Tex
import MarXup.MetaPost
import Data.List (intersperse)
import Data.Monoid hiding ((<>))

-- Separate the arguments with '\\'
mkrows :: [TeX] -> TeX
mkrows ls = sequence_ $ intersperse newline ls 

-- Separate the arguments with '&'
mkcols = sequence_ . intersperse newcol

vspace = cmd "vspace"
hspace = cmd "hspace"
title = cmd "title"

newline = backslash <> backslash
newcol = tex "&"
newpara = texLines ["",""]

maketitle :: Tex ()
maketitle = cmd "maketitle" $ return ()

ldots = cmd "ldots" (return ())

section s = 
  do cmd "section" s
     label

subsection s = 
  do cmd "subsection" s
     label

color :: String -> Tex a -> Tex a
color col bod = do 
  [_,x] <- cmdn' "textcolor" [] [tex col >> return undefined, bod]
  return x

----------------
-- Preamble stuff

usepackage opts name = cmd' "usepackage" opts (Tex name)

stdPreamble = do 
  usepackage [] "graphicx"
  usepackage ["mathletters"] "ucs"
  usepackage ["utf8x"] "inputenc"
  return ()

latexDocument :: String -> [String] -> Tex a -> Tex a -> Tex ()
latexDocument docClass options pre body = do
   preamble
   Metapost $ metaPostPreamble preamble
   env "document" body
   Metapost $ metaPostEpilogue
 where 
   preamble = do
     cmd' "documentclass" options (Tex docClass)
     pre

----------
-- Lists

item :: Tex a -> Tex a
item x = cmdn' "item" [] [] >> x

enumerate :: [Tex a] -> Tex [a]
enumerate [] = return [] -- latex does not like empty lists.
enumerate xs = env "enumerate" $ 
    mapM item xs

----------
-- Fonts

sf, em :: Tex a -> Tex a
sf = cmd "textsf"
em = cmd "emph"

----------
-- Math

align  = env "align*" . mkrows . map mkcols 

-- | A block
block :: [TeX] -> TeX
block  bod = do
  cmdn' "begin" [] [tex "array", tex "l"]
  mkrows $ bod
  cmdn' "end" [] [tex "array"]
  return ()

math = cmd "ensuremath"
mbox = cmd "mbox"

displayMath body = Tex "\\[" *> body <* Tex "\\]"

paren = parenthesize (tex "(") (tex ")")
brack = parenthesize (tex "[") (tex "]")
brac = parenthesize (backslash >> tex "{") (backslash >> tex "}")
bigBraces = bigParenthesize (backslash >> tex "{") (backslash >> tex "}")

bigParenthesize l r bod = do
  Tex "\\left" >> l
  x <- bod
  Tex "\\right" >> r
  return x
  
parenthesize l r bod = do
  l
  x <- bod
  r
  return x


mathsf = cmd "mathsf"

instance Fractional TeX where
    a / b = cmdn_ "frac" [a,b]

instance Floating TeX where
    pi = cmd "pi" nil
    exp x = "e^" <> braces x
    sqrt = cmd "sqrt"

instance Num TeX where
  fromInteger x = text $ show x
  (+) = binop $ text "+"
  (-) = binop $ text "-"
  (*) = binop $ text "*"
  negate x = "-" <> x

binop :: TeX -> TeX -> TeX -> TeX
binop op a b = a <> op <> b

