
module MarXup.MetaPost where

import MarXup.Tex

  
metaPostPreamble :: Tex a -> Tex ()
metaPostPreamble texPreamble =  do
  texLines ["outputtemplate := \"%j-%c.mps\";",
            "input drv;",
            "verbatimtex %&latex",
            ""]
  texPreamble
  texLines ["",
            "\\begin{document}",
            "etex;",
            "prologues:=3;"]  
  
metaPostEpilogue = texLn "end"
    

metapostFigure :: Label -> Tex a -> Tex ()
metapostFigure lab t = Metapost $ do
  Tex "beginfig(" >> Refer lab >> Tex ")\n"
  t
  texLn "endfig;"

