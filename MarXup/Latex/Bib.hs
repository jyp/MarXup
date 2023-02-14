{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module MarXup.Latex.Bib where
import MarXup 
import MarXup.Tex 
import MarXup.Verbatim
import Data.Char (toUpper)
-------------
-- Bib

data CitationPart =
  CitationPart {
    citKey :: String,
    citPostnote, citPrenote :: TeX
  }

data CitationHow = CiteAuthor | CiteParen | CiteText deriving (Eq,Ord)

data Citation = Citation {citationCapitalize :: Bool,
                          citationHow :: CitationHow,
                          citationParts :: [CitationPart]}

instance Semigroup Citation where
  Citation a1 b1 c1 <> Citation a2 b2 c2 = Citation (a1 && a2) (max b1 b2) (c1 <> c2)

bibliographystyle :: String -> TeX
bibliographystyle x = cmd "bibliographystyle" $ tex x

bibliography :: String -> TeX
bibliography x = do
  texLn "" -- some tools can detect bibliography command only if it stands on its own line.
  cmd "bibliography" $ tex x
  texLn ""

citation :: Citation -> TeX
citation (Citation c how parts) = 
  cmdGeneric cmdName (concatMap partArgs parts) >>
  return ()
  where
    partArgs CitationPart{..} = 
      (brackets <$> [citPrenote,citPostnote]) ++ [braces (tex citKey)]
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
  element = citation
  type Target Citation = TeX

postnote :: Citation -> TeX -> Citation
postnote Citation{..} note =
  Citation{citationParts=fmap (\CitationPart{..} -> CitationPart{citPostnote = note,..}) citationParts,..}

prenote :: TeX -> Citation -> Citation
prenote note Citation{..} =
  Citation{citationParts=fmap (\CitationPart{..} -> CitationPart{citPostnote = note,..}) citationParts,..}

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


printBibliography :: Tex ()
printBibliography = cmd0 "printbibliography"

