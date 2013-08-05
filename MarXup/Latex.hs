{-# LANGUAGE OverloadedStrings #-}
module MarXup.Latex where

import Control.Monad (forM_)
import MarXup.Tex
import MarXup.MetaPost
import Data.List (intersperse,groupBy)
import Data.Monoid
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
        emails = mconcat $ intersperse (cmd0 "and") $ map (textual . scnd) g
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
  fmt <- getMpOutFormat
  case fmt of
    SVG -> usepackage "svg" [] 
    EPS -> usepackage "graphicx" []
  usepackage "inputenc" ["utf8"] 
  return ()

documentClass :: String -> [String] -> TeX
documentClass docClass options = cmd' "documentclass" options (tex docClass)

latexDocument :: (Bool -> TeX) -> Tex a -> Tex ()
latexDocument preamble body = do
   fmt <- getMpOutFormat
   preamble False
   inMP $ metaPostPreamble fmt (preamble True)
   env "document" body
   inMP $ metaPostEpilogue

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

displayMath = env "displaymath"
  -- tex "\\[" *> body <* tex "\\]"

paren,brack,brac,bigBrac,bigParen :: Tex a -> Tex a
paren = parenthesize (tex "(") (tex ")")
brack = parenthesize (tex "[") (tex "]")
brac = parenthesize (backslash >> tex "{") (backslash >> tex "}")
bigBrac = bigParenthesize (backslash >> tex "{") (backslash >> tex "}")
bigParen = bigParenthesize (tex "(") (tex ")")

parenthesize,bigParenthesize :: TeX -> TeX -> Tex a -> Tex a
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

mathsf :: Tex a -> Tex a
mathsf = cmd "mathsf"

