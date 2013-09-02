module MarXup.Latex.Bib where
import MarXup.Tex 

-------------
-- Bib

bibliographystyle x = cmd "bibliographystyle" $ textual x
bibliography x = cmd "bibliography" x 

citet,citep :: TeX -> TeX
[citet,citep] = map cmd ["citet","cite"]

