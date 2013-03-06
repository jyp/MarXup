{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module MarXup.MetaPost where

import MarXup.Tex
import MarXup.MultiRef
import Control.Monad.Reader
import Control.Applicative
import GHC.Exts( IsString(..) )
import Data.Monoid
import System.FilePath
  
newtype MP a = MP (Multi a)
  deriving (Monad, MonadFix, Applicative, Functor)

instance IsString (MP ()) where
   fromString = MP . Raw

instance Monoid (MP()) where  
  mempty = MP $ Raw ""
  mappend = (>>)

mpRawLines :: [String] -> MP ()
mpRawLines ls = MP $ Raw $ concat $ map (++"\n") ls 

mpRefer l = sho =<< MP (Refer l)
mpLabel  = MP $ Label

sho :: Show a => a -> MP ()
sho = MP . Raw . show
                             

mpRawTex :: Tex a -> MP a
mpRawTex (Tex t) = MP (runReaderT t "<in metapost>")

metaPostPreamble :: Tex a -> MP ()
metaPostPreamble texPreamble =  do
  mpRawLines ["outputtemplate := \"%j-%c.mps\";",
            "input drv;",
            "verbatimtex %&latex",
            ""]
  mpRawTex texPreamble
  mpRawLines ["",
            "\\begin{document}",
            "etex;",
            "prologues:=3;"]  
  
metaPostEpilogue = mpRawLines ["end"]
    

mkfig :: Label -> MP () -> MP ()
mkfig lab (MP t) = MP $ do
  Raw "beginfig(" >> Refer lab >> Raw ")\n"
  t
  Raw "endfig;\n"

inMP :: MP a -> Tex a
inMP (MP mp) = do
 fname <- ask
 Tex $ lift $ Target (fname <.> "mp") mp

mpQuote :: TeX -> MP ()
mpQuote t = "\"" <> mpRawTex t <> "\""


createMetaPostFigure :: Label -> MP () -> Tex ()
createMetaPostFigure lab t = do 
  inMP $ mkfig lab t

includeMetaPostFigure :: [String] -> Label -> TeX
includeMetaPostFigure opts l = do
  fname <- ask 
  let figname = fname ++ "-" ++ show l ++ ".mps"
  cmd' "includegraphics" opts (tex figname)
    
