{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module MarXup.Latex.Bib (postnote, prenote, addBibResources, bibliographySection, useBiblatex, Citation,
                         nominative, cites, citep, citet, citeauthor) where
import MarXup 
import MarXup.Tex 
import MarXup.Verbatim
import Data.Char (toUpper)
import Data.List (intercalate)
import MarXup.MultiRef (metaData,getMetaDataOptions)
import Data.Maybe
import Data.Foldable

-------------
-- Bib

data CitationPart =
  CitationPart {
    citKey :: String,
    citPostnote, citPrenote :: Maybe TeX
  }

data CitationHow = CiteAuthor | CiteParen | CiteText deriving (Eq,Ord)

data Citation = Citation {citationCapitalize :: Bool,
                          citationHow :: CitationHow,
                          citationParts :: [CitationPart]}

instance Semigroup Citation where
  Citation a1 b1 c1 <> Citation a2 b2 c2 = Citation (a1 && a2) (max b1 b2) (c1 <> c2)
       

postnote :: Citation -> TeX -> Citation
postnote Citation{..} note =
  Citation{citationParts=fmap (\CitationPart{..} -> CitationPart{citPostnote = Just note,..}) citationParts,..}

prenote :: TeX -> Citation -> Citation
prenote note Citation{..} =
  Citation{citationParts=fmap (\CitationPart{..} -> CitationPart{citPostnote = Just note,..}) citationParts,..}

-- | Force citation style to be nominative ("textcite")
nominative :: Citation -> Citation
nominative (Citation caps _ parts) = Citation caps CiteText parts

citet :: Verbatim a -> Citation
citet (Verbatim x _) = Citation False CiteText [CitationPart x mempty mempty] 

citep :: Verbatim a -> Citation
citep (Verbatim x _) = Citation False CiteParen [CitationPart x mempty mempty] 

cites :: [Verbatim a] -> Citation
cites vs = Citation False CiteParen [CitationPart (fromVerbatim x) mempty mempty | x <- vs] 

citeauthor :: Verbatim a -> Citation
citeauthor (Verbatim x _) = Citation False CiteAuthor [CitationPart x mempty mempty] 


bibliographystyle :: String -> TeX
bibliographystyle x = cmd "bibliographystyle" $ tex x

bibliography :: [String] -> TeX
bibliography bibfiles = do
  texLn "" -- some tools can detect bibliography command only if it stands on its own line.
  cmd "bibliography" $ (tex $ intercalate "," bibfiles)
  texLn ""

renderNatbib :: Citation -> TeX
renderNatbib (Citation c how parts) = do
  case parts of
    [] -> mempty
    [CitationPart k Nothing Nothing] -> cmd cmdName (tex k)
    [CitationPart k Nothing (Just post)] -> cmdm cmdName [post] [tex k] >> return ()
    [p] -> cmdGeneric cmdName (partArgs p) >> return ()
    _ -> cmd cmdName (tex (intercalate "," $ fmap citKey parts)) -- prenote and postnote not supported by natbib for multiple keys.
  return ()
  where
    cmdName =
      maybeCapitalize $
      case how of
        CiteAuthor -> "citeauthor"
        CiteParen -> "citep"
        CiteText -> "citet"
    maybeCapitalize = \case
      [] -> []
      (x:xs) -> (if c then toUpper else id) x : xs

noteOption :: Maybe (TeX) -> TeX
noteOption note = brackets (maybe mempty id note)

partArgs :: CitationPart -> [TeX]
partArgs CitationPart{..} = (noteOption <$> [citPrenote, citPostnote]) ++ [braces (tex citKey)]

renderBiblatex :: Citation -> TeX
renderBiblatex (Citation c how parts) = do
  cmdGeneric cmdName (concatMap partArgs parts) >> return ()
  where
    cmdName =
      (++ ['s' | multi]) $
      maybeCapitalize $
      case how of
        CiteAuthor -> "citeauthor"
        CiteParen -> "parencite"
        CiteText -> "textcite"
    multi = length parts > 1
    maybeCapitalize = \case
      [] -> []
      (x:xs) -> (if c then toUpper else id) x : xs

instance Element Citation where
  element c = do
    isBiblatex <- isBiblatexMode
    if isBiblatex then renderBiblatex c else renderNatbib c
  type Target Citation = TeX

useBiblatex :: [String] -> Tex ()
useBiblatex options = do
  Tex $ metaData Biblatex options
  bibfiles <- getBibResources
  forM_ bibfiles $ \f ->
    cmd "addbibresource" (tex f)

isBiblatexMode :: Tex Bool
isBiblatexMode = do
  isJust <$> (Tex $ getMetaDataOptions Biblatex)

printBibliography :: Tex ()
printBibliography = cmd0 "printbibliography"

-- | bibstyle argument is ignored in biblatex mode.
bibliographySection :: String -> Tex ()
bibliographySection bibstyle = do
  m <- isBiblatexMode
  if m then printBibliography else do
    bibfiles <- getBibResources
    bibliography bibfiles
    bibliographystyle bibstyle

addBibResources :: [String] -> TeX
addBibResources = Tex . metaData BibResource

getBibResources :: Tex [String]
getBibResources = do
    maybeBibfiles <- Tex $ getMetaDataOptions BibResource
    return (maybe [] toList maybeBibfiles)
