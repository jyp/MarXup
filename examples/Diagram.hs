{-# OPTIONS_GHC -XOverloadedStrings -XDoRec -pgmF marxup -F #-}

import MarXup.Latex
import MarXup.Tex
import MarXup.MetaPost
import MarXup.Diagrams
import Control.Applicative

preamble :: Tex ()
preamble = do
  usepackage ["mathletters"] "ucs"
  usepackage ["utf8x"] "inputenc"
  usepackage [] "graphicx"

diagram = mpFigure [] $ do 
  t <- textObj "checkit"
  b <- boxObj 
  E ▸ t === W ▸ b
  return ()

main = render $ latexDocument "article" ["11pt"] preamble $ @"
Here it comes:

@diagram
@"

