module MarXup.Latex.Bib where
import MarXup.Tex 

-------------
-- Bib


bibliographystyle :: String -> TeX
bibliographystyle x = cmd "bibliographystyle" $ tex x

bibliography :: String -> TeX
bibliography x = cmd "bibliography" $ tex x

citet,citep :: String -> TeX
[citet,citep] = map (\c -> cmd c . tex) ["citet","citep"]

