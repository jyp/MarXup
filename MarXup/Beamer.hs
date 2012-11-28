{-# LANGUAGE OverloadedStrings #-}
module MarXup.Beamer where

import Control.Applicative
import Data.Monoid
import MarXup.Tex

usetheme = cmd "usetheme" . tex

frame tit bod = env "frame" $ do
  cmd "frametitle" tit
  bod
  
pause :: TeX
pause = cmd "pause"$  mempty

note = cmd "note"

{-
preamble :: Tex ()
preamble = do 
  usepackage [] "bbm"
  usetheme "Malmoe"
  
  title $ tex "Elements of Parametricity and Computational Irrelevance" 
  cmd' "author" ["JP Bernardy"] $ tex "Jean-Philippe Bernardy"
  cmd "institute" $ tex "Chalmers University of Technology and University of Gothenburg"
  cmd "date" $ cmd "today" mempty
-}