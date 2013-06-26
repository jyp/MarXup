{-# LANGUAGE OverloadedStrings #-}
module MarXup.Latex where

import Control.Applicative
import Control.Monad (forM_)
import MarXup.Tex
import MarXup.MetaPost
import Data.List (intersperse,groupBy)
import Data.Monoid
import MarXup.MultiRef
import Data.Function (on)


-- | Separate the arguments with '\\'
mkrows,mkcols :: [TeX] -> TeX
mkrows ls = sequence_ $ intersperse newline ls 

-- | Separate the arguments with '&'
mkcols = sequence_ . intersperse newcol

vspace, hspace :: String -> TeX 
vspace = cmd "vspace" . textual
hspace = cmd "hspace" . textual

title :: TeX -> TeX
title = cmd "title" 

data AuthorInfoStyle = Plain | LNCS | SIGPlan | IEEE

-- | author info in as triplets name, institution, email
authorinfo :: AuthorInfoStyle -> [(String,String,String)] -> TeX
authorinfo Plain as = cmd "author" $ mconcat $ intersperse (cmd0 "and") $ map oneauthor as
  where oneauthor (name,_,institution) = textual name <> newline <> textual institution
                                   
authorinfo SIGPlan as = forM_ (groupBy ((==) `on` thrd) as) $ \ (g@((_,_,institution):_)) -> do
    let names = map frst g
        emails = mconcat $ map (textual . scnd) g
    cmdn "authorinfo" [mconcat $ intersperse (cmd0 "and") $ map textual names, textual institution, emails]
    return ()
  where 
  scnd (_,x,_) = x
  frst (x,_,_) = x
  thrd (_,_,x) = x
  

newline = backslash <> backslash
newcol = tex "&"
newpara = texLines ["",""]

maketitle :: Tex ()
maketitle = cmd "maketitle" $ return ()

ldots :: TeX
ldots = cmd "ldots" (return ())

-- | Sectioning
section,subsection,paragraph :: TeX -> Tex SortedLabel
section s = cmd "section" s >> label "Sec."
subsection s = cmd "subsection" s >> label "Sec."
paragraph s = cmd "paragraph" s >> label "Sec."

color :: String -> Tex a -> Tex a
color col bod = do 
  [_,x] <- cmdn' "textcolor" [] [tex col >> return undefined, bod]
  return x

----------------
-- Preamble stuff


usepackage :: String -> [String] -> Tex ()
usepackage name opts  = cmd' "usepackage" opts (tex name)

stdPreamble :: TeX
stdPreamble = do 
  usepackage "graphicx" [] -- used for import of metapost diagrams
  usepackage "inputenc" ["utf8"] 
  return ()

latexDocument :: String -> [String] -> (Bool -> TeX) -> Tex a -> Tex ()
latexDocument docClass options pre body = do
   preamble False
   inMP $ metaPostPreamble (preamble True)
   env "document" body
   inMP $ metaPostEpilogue
 where 
   preamble inMetaPost = do
     cmd' "documentclass" options (tex docClass)
     pre inMetaPost

----------
-- Lists

item = cmd0 "item"
enumerate = env "enumerate" 
itemize = env "itemize" 

------------------------
-- Various environments


figure :: TeX -> TeX -> Tex SortedLabel
figure caption body = env "figure" $ do
  body
  cmd "caption" caption
  label "Fig."

----------
-- Fonts

sans, emph, smallcaps :: Tex a -> Tex a
sans = cmd "textsf"
emph = cmd "emph"

smallcaps x = braces (cmd0 "sc" >> x)

----------
-- Math

align  = env "align*" . mkrows . map mkcols 

array :: [String] -> TeX -> [[TeX]] -> TeX
array opts format bod = math $ do
  env' "array" opts $ do
    braces format
    mkrows (map mkcols bod)
  return ()

-- | A block
block :: [TeX] -> TeX
block  bod = do
  env "array" $ do
    braces (tex "l") 
    mkrows $ bod
  return ()

displayMath,math,mbox :: Tex a -> Tex a
math = cmd "ensuremath"
mbox = cmd "mbox"

displayMath body = tex "\\[" *> body <* tex "\\]"

paren = parenthesize (tex "(") (tex ")")
brack = parenthesize (tex "[") (tex "]")
brac = parenthesize (backslash >> tex "{") (backslash >> tex "}")
bigBraces = bigParenthesize (backslash >> tex "{") (backslash >> tex "}")

bigParenthesize l r bod = do
  tex "\\left" >> l
  x <- bod
  tex "\\right" >> r
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
  fromInteger x = textual $ show x
  (+) = binop $ textual "+"
  (-) = binop $ textual "-"
  (*) = binop $ textual "*"
  negate x = "-" <> x

binop :: TeX -> TeX -> TeX -> TeX
binop op a b = a <> op <> b

