module MarXup.Latex.Bib where
import MarXup.Tex 

-------------
-- Bib


bibliographystyle :: String -> TeX
bibliographystyle x = cmd "bibliographystyle" $ tex x

bibliography :: String -> TeX
bibliography x = do
  texLn "" -- some tools can detect bibliography command only if it stands on its own line.
  cmd "bibliography" $ tex x
  texLn ""

citet,citep :: String -> TeX
[citet,citep] = map (\c -> cmd c . tex) ["citet","citep"]

