{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module MarXup.Latex where

import MarXup
import Control.Monad (forM_)
import MarXup.Tex
import MarXup.MetaPost
import Data.List (intersperse,groupBy,elemIndex,nub)
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


data ClassFile = Plain | LNCS | SIGPlan | IEEE | Beamer
  deriving Eq
type AuthorInfoStyle = ClassFile

data AuthorInfo = AuthorInfo {authorName :: String, authorEmail :: String, authorInst :: String}

-- | author info in as triplets name, institution, email
authorinfo :: AuthorInfoStyle -> [AuthorInfo] -> TeX
authorinfo LNCS as = do
  cmd "author" $ mconcat $ intersperse (cmd0 "and") $ map oneauthor as
  cmd "institute" $ mconcat $ intersperse (cmd0 "and") $ map textual $ insts
  where oneauthor AuthorInfo{..} = textual authorName <> (if length insts > 1 then cmd "inst" (textual $ show $ 1 + instIdx) else mempty)
           where Just instIdx = elemIndex authorInst insts
        insts = nub $ map authorInst as

authorinfo SIGPlan as = forM_ (groupBy ((==) `on` authorInst) as) $ \ (g@((AuthorInfo _ _ institution):_)) -> do
    let names = map authorName g
        emails = mconcat $ intersperse (cmd0 "and") $ map (textual . authorEmail) g
    cmdn "authorinfo" [mconcat $ intersperse (cmd0 "and") $ map textual names, textual institution, emails]
    return ()
authorinfo IEEE as = cmd "author" $ do
  cmd "IEEEauthorblockN" $ mconcat $ intersperse (hspace "1cm") $ map (textual . authorName) as
  tex "\n\n" -- for some reason the IEEE class wants a paragraph separation here.
  cmd "IEEEauthorblockA" $ mkrows $ [textual inst,"email: " <> textual (mconcat $ intersperse " " $ map authorEmail as)]
  where (AuthorInfo {authorInst = inst}:_) = as
authorinfo _ {- Plain, Beamer -} as = cmd "author" $ mconcat $ intersperse (cmd0 "and") $ map oneauthor as
  where oneauthor (AuthorInfo name _ institution) = textual name <> newline <> textual institution

keywords :: ClassFile -> [String] -> TeX
keywords LNCS ks = do
  cmd "keywords" $ mconcat $ intersperse ", " $ map textual ks
keywords IEEE ks = env "IEEEkeywords" $ do
  mconcat $ intersperse ", " $ map textual ks
keywords SIGPlan ks = do
  cmd0 "keywords"
  mconcat $ intersperse ", " $ map textual ks


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

figure_ :: TeX -> TeX -> Tex SortedLabel
figure_ caption body = env "figure*" $ do
  body
  cmd "caption" caption
  label "Fig."

figure :: TeX -> TeX -> Tex SortedLabel
figure caption body = env "figure" $ do
  body
  cmd "caption" caption
  label "Fig."

----------
-- Fonts

data TextSize = Tiny | ScriptSize | FootnoteSize | Small | NormalSize | Large | Larger | Largerr | Huge | Huger

textSize :: TextSize -> Tex a -> Tex a
textSize sz x = braces (cmd0 latexSize >> x)
  where latexSize = case sz of
          Tiny -> "tiny"
          ScriptSize -> "scriptsize"
          FootnoteSize -> "footnotesize"
          Small -> "small"
          NormalSize -> "normalsize"
          Large -> "large"
          Larger -> "Large"
          Largerr -> "LARGE"
          Huge -> "huge"
          Huger -> "Huge"



sans, emph, smallcaps :: Tex a -> Tex a
sans = cmd "textsf"
emph = cmd "emph"

smallcaps x = braces (cmd0 "sc" >> x)

italic :: Tex a -> Tex a
italic = cmd "textit"

-------------------------
-- Scaling and rotating

scalebox :: Double -> Tex a -> Tex a
scalebox factor x = do
  cmd "scalebox" $ tex $ show factor
  braces x

----------
-- Math

align  = env "align*" . mkrows . map mkcols

array :: [String] -> String -> [[TeX]] -> TeX
array opts format bod = math $ do
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

inferrule :: [TeX] -> TeX -> TeX
inferrule xs y = cmdn "inferrule" [mkrows xs,y] >> return ()

