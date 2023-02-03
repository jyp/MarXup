{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module MarXup.Latex where

import MarXup
import MarXup.Verbatim
import Control.Monad (forM_,when,forM,unless)
import MarXup.Tex
import Data.List (intersperse,groupBy,elemIndex,nub)
import Data.Function (on)

-- | Separate the arguments with '\\'
mkrows,mkcols :: [TeX] -> TeX
mkrows ls = sequence_ $ intersperse newline ls

-- | Separate the arguments with '&'
mkcols = sequence_ . intersperse newcol

vspace, hspace :: String -> TeX
vspace = cmd "vspace" . tex
hspace = cmd "hspace" . tex

hfill :: TeX
hfill = cmd0 "hfill"

title :: TeX -> TeX
title = cmd "title"

type AuthorInfoStyle = ClassFile
 
data Affiliation = Affiliation {affilInstitution :: String, affilCity :: String, affilCountry :: String}

data AuthorInfo = AuthorInfo {authorName :: String, authorEmail :: String, authorAffil :: Affiliation}

authorinfo :: [AuthorInfo] -> Tex ()
authorinfo as = do c<-askClass; authorinfo' c as

authorInst :: AuthorInfo -> String
authorInst = affilInstitution . authorAffil

-- | author info as triplets name, institution, email
authorinfo' :: AuthorInfoStyle -> [AuthorInfo] -> TeX
authorinfo' ACMArt as = forM_ as $ \AuthorInfo{authorAffil = Affiliation{..},..} -> do
  cmd "author" $ textual authorName
  cmd "affiliation" $ do
    cmd "institution" $ textual affilInstitution
    cmd "city" $ textual affilCity
    cmd "country" $ textual affilCountry
  cmd "email" $ textual authorEmail
authorinfo' LNCS as = do
  cmd "author" $ mconcat $ intersperse (cmd0 "and") $ map oneauthor as
  cmd "institute" $ mconcat $ intersperse (cmd0 "and") $ map textual $ insts
  where oneauthor AuthorInfo{authorAffil = Affiliation {affilInstitution = institution},..} = textual authorName <> (if length insts > 1 then cmd "inst" (textual $ show $ 1 + instIdx) else mempty)
           where Just instIdx = elemIndex institution insts
        insts = nub $ map authorInst as
authorinfo' SIGPlan as = forM_ (groupBy ((==) `on` authorInst) as) $ \ (g@((AuthorInfo _ _ Affiliation {affilInstitution = institution}):_)) -> do
    let names = map authorName g
        emails = mconcat $ intersperse (cmd0 "and") $ map (textual . authorEmail) g
    _ <- cmdn "authorinfo" [mconcat $ intersperse (cmd0 "and") $ map textual names, textual institution, emails]
    return ()
authorinfo' EPTCS as = mconcat $ intersperse and' $
  flip map (groupBy ((==) `on` authorInst) as) $ \ (g@((AuthorInfo _ _ Affiliation {affilInstitution = institution}):_)) -> do
    cmd "author" $ do
      mconcat $ intersperse dquad $ map (textual . authorName) g
      cmd "institute" $ textual institution
      cmd "email" $ mconcat $ intersperse dquad $ map (textual . authorEmail) g
    return ()
  where dquad = cmd0 "quad" <> cmd0 "quad"
        and' = cmd0 "and"
authorinfo' Beamer as = do
  cmd "author" $ mconcat $ intersperse (cmd0 "and") $ flip map as $ \a -> do
      textual $ authorName a
      case elemIndex (authorInst a) institutions of
        Nothing -> textual $ "error: could not find " ++ (authorInst a)
        Just idx -> inst idx
      -- No emails in beamer
  cmd "institute" $
      forM_ (zip institutions [0..]) $ \(i,idx) -> do
        inst idx
        textual i
  return ()
  where institutions = nub $ map authorInst $ as
        inst :: Int -> TeX
        inst i = when (length institutions > 1) $ cmd "inst" $ tex $ show (i+1)

authorinfo' IEEE as = cmd "author" $ do
  cmd "IEEEauthorblockN" $ mconcat $ intersperse (hspace "1cm") $ map (textual . authorName) as
  tex "\n\n" -- for some reason the IEEE class wants a paragraph separation here.
  cmd "IEEEauthorblockA" $ mkrows $ [textual inst,"email: " <> textual (mconcat $ intersperse " " $ map authorEmail as)]
  where (AuthorInfo {authorAffil = Affiliation {affilInstitution = inst}}:_) = as
authorinfo' _ {- Plain -} as = cmd "author" $ mconcat $ intersperse (cmd0 "and") $ map oneauthor as
  where oneauthor (AuthorInfo name _ Affiliation {affilInstitution = institution}) = textual name <> newline <> textual institution

keywords :: [String] -> TeX
keywords ks = do
  classFile <- askClass
  case classFile of
    Plain -> do
      paragraph "keywords"
      mconcat $ intersperse ", " $ map textual ks
      return ()
    LNCS -> do cmd "keywords" $ mconcat $ intersperse ", " $ map textual ks
    IEEE -> env "IEEEkeywords" $ do mconcat $ intersperse ", " $ map textual ks
    SIGPlan -> do cmd0 "keywords"
                  mconcat $ intersperse ", " $ map textual ks
    _ -> return ()

acknowledgements :: Tex a -> Tex a
acknowledgements body = do
  classFile <- askClass
  case classFile of
    SIGPlan -> cmd0 "acks" >> body
    ACMArt -> do
      x <- env "acks" $ do
        texLn "" -- some idiotic bug in acmart
        body
      texLn "" -- some idiotic bug in acmart
      texLn ""
      return x
    _ -> do paragraph "acknowledgements"
            body


footnote :: Tex a -> Tex a
footnote = cmd "footnote"


newline :: TeX
newline = backslash <> backslash
newcol :: TeX
newcol = tex "&"
newpara :: Tex ()
newpara = texLn "\\par"
newpage :: TeX
newpage = cmd0 "newpage"

maketitle :: Tex ()
maketitle = cmd "maketitle" $ return ()

ldots :: TeX
ldots = cmd "ldots" (return ())

-- | Sectioning
section,subsection,subsubsection,paragraph :: TeX -> Tex SortedLabel
section s = cmd "section" s >> label "Sec."
subsection s = cmd "subsection" s >> label "Sec."
subsubsection s = cmd "subsubsection" s >> label "Sec."
paragraph s = cmd "paragraph" s >> label "Sec."

color :: String -> Tex a -> Tex a
color col bod = do
  xs <- cmdn' "textcolor" [] [tex col >> return undefined, bod]
  case xs of
    [_,x] -> return x
    _ -> error "MarXup.Tex.color: the impossible happened"

----------------
-- Preamble stuff


usepackage :: String -> [String] -> Tex ()
usepackage name opts  = cmd' "usepackage" opts (tex name)

stdPreamble :: TeX
stdPreamble = do
  usepackage "graphicx" []
  usepackage "ifxetex" []
  cmd0 "ifxetex"
  usepackage "fontspec" []
  usepackage "newunicodechar" []
  mapM_ texLn ["\\newcommand{\\DeclareUnicodeCharacter}[2]{%"
              ,"\\begingroup\\lccode`|=\\string\"#1\\relax"
              ,"\\lowercase{\\endgroup\\newunicodechar{|}}{#2}%"
              ,"}"]
  cmd0 "else"
  usepackage "inputenc" ["utf8"]
  cmd0 "fi"
  return ()

----------
-- Lists

{-# DEPRECATED item, enumerate, itemize "Since Aug 2015. Use itemList, enumList, descList instead "#-}
item :: Tex ()
item = cmd0 "item"
enumerate :: Tex a -> Tex a
enumerate = env "enumerate"
itemize :: Tex a -> Tex a
itemize = env "itemize"

itemList :: [TeX] -> TeX
itemList [] = mempty
itemList xs = env "itemize" $ forM_ xs $ \x -> do
  cmd0 "item"
  x

enumList :: [TeX] -> TeX
enumList [] = mempty
enumList xs = env "enumerate" $ forM_ xs $ \x -> do
  cmd0 "item"
  x

descList :: [(TeX,TeX)] -> TeX
descList [] = mempty
descList xs = env "enumerate" $ forM_ xs $ \(lab,x) -> do
  cmdm "item" [lab] []
  x

------------------------
-- Various environments

tabular :: [String] -> String -> [[TeX]] -> TeX
tabular opts format bod = do
  env' "tabular" opts $ do
    braces (tex format)
    mkrows (map mkcols bod)
  return ()

tabularx :: [String] -> String -> [[TeX]] -> TeX
tabularx opts format bod = do
  usepkg "tabularx" 100 [] 
  env' "tabularx" opts $ do
    braces (tex format)
    mkrows (map mkcols bod)
  return ()

multicols :: Int -> Tex a -> Tex a
multicols n body = do
  usepkg "multicol" 100 []
  env'' "multicols" [] [tex (show n)] body

makecell :: String -> Tex b -> Tex b
makecell opts body = do
  usepkg "makecell" 100 []
  cmd' "makecell" [opts] body

center ::  Tex a -> Tex a
center = env "center"

figure_ :: TeX -> Tex a -> Tex (a, SortedLabel)
figure_ = figAny "figure*" []

figure :: TeX -> Tex a -> Tex (a,SortedLabel)
figure = figAny "figure" []

data FloatPlacement = PlaceHere | PlaceHERE | PlaceForce | PlaceTop | PlacePage| PlaceBottom | PlacementOverride

showPlacement :: FloatPlacement -> String
showPlacement = \case
  PlaceBottom -> "b"
  PlacementOverride -> "!"
  PlaceHERE -> "H"
  PlaceTop -> "t"
  PlacePage -> "p"
  PlaceHere -> "h"

figAny :: String -> [FloatPlacement] -> TeX -> Tex a -> Tex (a,SortedLabel)
figAny theEnv place caption body = env' theEnv [concatMap showPlacement place] $ do
  x <- body
  cmd "caption" caption
  (x,) <$> label "Fig."

wrapfigure :: Bool -> Size -> TeX -> Tex a -> Tex (a,SortedLabel)
wrapfigure isRight sz caption body = do
  usepkg "wrapfig" 100 []
  env'' "wrapfigure" [] [if isRight then "r" else "l", texSize sz]  $ do
    x <- body
    cmd "caption" caption
    (x,) <$> label "Fig."

subfigure :: String -> Size -> TeX -> TeX -> Tex SortedLabel
subfigure option width caption body = do
  usepkg "subcaption" 100 []
  env'' "subfigure" [tex option] [texSize width]  $ do
    body
    cmd "caption" caption
    label "Subfig."

subfigures :: TeX -> [(Size,TeX,TeX)] -> Tex ([SortedLabel],SortedLabel)
subfigures caption subs = figure_ caption $
  forM (zip (True:repeat False) subs) $ \(isFirst,(sz,subcaption,body)) -> do
    unless isFirst (cmd0 "quad")
    subfigure "b" sz subcaption body

table :: TeX -> TeX -> Tex SortedLabel
table caption body = env "table" $ do
  cmd "caption" caption
  body
  MarXup.Tex.label "Table"

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



bold,sans, emph, smallcaps :: Tex a -> Tex a
sans = cmd "textsf"
emph = cmd "emph"
bold = cmd "textbf"

smallcaps x = braces (cmd0 "sc" >> x)

italic :: Tex a -> Tex a
italic = cmd "textit"

teletype :: Tex a -> Tex a
teletype = cmd "texttt"

small ::  Tex a -> Tex a
small x = braces (cmd0 "small" >> x)

url :: Tex a -> Tex a
url = cmd "url"

-------------------------
-- Scaling and rotating

scalebox :: Double -> Tex a -> Tex a
scalebox factor x = do
  cmd "scalebox" $ tex $ show factor
  braces x

-----------
-- Parens
qu :: TeX -> TeX
qu x = tex "``" <> x <> tex "''"


inferrule' :: TeX -> [TeX] -> TeX -> TeX
inferrule' name xs y = cmdm "inferrule" [name] [mkrows xs,y] >> return ()

inferrule :: [TeX] -> TeX -> TeX
inferrule xs y = cmdn "inferrule" [mkrows xs,y] >> return ()

----------------------
-- Listings

listing :: [String] -> Verbatim () -> TeX
listing opt (Verbatim s _) =
    env' "lstlisting" opt (tex s)

lstinline :: [String] -> Verbatim () -> TeX
lstinline opt (Verbatim s _) =
    let sep = tex "$"
        opt' = tex $ mconcat . intersperse ", "
               $ "basicstyle=\\ttfamily" :  opt in
    backslash <> "lstinline" <> brackets opt' <> sep <> tex s <> sep

