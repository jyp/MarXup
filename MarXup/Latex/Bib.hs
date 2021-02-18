module MarXup.Latex.Bib where
import MarXup.Tex 
import MarXup.Verbatim

-------------
-- Bib


bibliographystyle :: String -> TeX
bibliographystyle x = cmd "bibliographystyle" $ tex x

bibliography :: String -> TeX
bibliography x = do
  texLn "" -- some tools can detect bibliography command only if it stands on its own line.
  cmd "bibliography" $ tex x
  texLn ""


citet' :: String -> Verbatim a -> Tex a
citet' opt (Verbatim x r) = cmd' "citet" [opt] (tex x) >> return r
citep' :: String -> Verbatim a -> Tex a
citep' opt (Verbatim x r) = cmd' "citep" [opt] (tex x) >> return r

citet :: Verbatim a -> Tex a
citet (Verbatim x r) = cmd "citet" (tex x) >> return r

citep :: Verbatim a -> Tex a
citep (Verbatim x r) = cmd "citep" (tex x) >> return r
