{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, EmptyDataDecls #-}

module MarXup.MetaPost where

import MarXup.Tex
import MarXup.MultiRef
import Control.Monad.Reader
import Control.Applicative
import GHC.Exts( IsString(..) )
import Data.Monoid
import System.FilePath
import Data.Ratio  
  
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
            "verbatimtex%&latex",
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

mpTex :: TeX -> MP ()
mpTex t = "btex " <> mpRawTex t <> " etex"

createMetaPostFigure :: Label -> MP () -> Tex ()
createMetaPostFigure lab t = do 
  inMP $ mkfig lab t

includeMetaPostFigure :: [String] -> Label -> TeX
includeMetaPostFigure opts l = do
  fname <- ask 
  let figname = fname ++ "-" ++ show l ++ ".mps"
  cmd' "includegraphics" opts (tex figname)

-- | Create and include a metapost drawing in one go. The 1st argument
-- are the options to includegraphics
mpFigure :: [String] -> MP () -> Tex Label
mpFigure opts mp = do
  l <- label
  createMetaPostFigure l mp
  includeMetaPostFigure opts l
  return l
    
-----------------------
-- Typed METAPOST

newtype Expr a = Expr {fromExpr :: String}
  deriving (Monoid,IsString)
instance Show (Expr a) where
  show (Expr s) = s
  
constant :: Rational -> Expr Numeric
constant r = Expr $ show r

data Numeric 
data Pair 
data Picture

instance Num (Expr a) where
  fromInteger x = Expr $ show x
  x + y = parens (x <> "+" <> y) 
  x - y = parens (x <> "-" <> y)
  x * y = parens (x <> "*" <> y)
  
(+:) :: Expr Numeric -> Expr Numeric -> Expr Pair
(Expr x) +: (Expr y) = Expr $ parens (x<>","<>y)
xpart,ypart :: Expr Pair -> Expr Numeric
xpart (Expr x) = Expr $ parens ("xpart " <> x)
ypart (Expr x) = Expr $ parens ("ypart " <> x)

parens x = "(" <> x <> ")"

(.*) :: Expr Numeric -> Expr Pair -> Expr Pair
Expr x .* Expr y = Expr $ x <> y

center :: [Expr Pair] -> Expr Pair
center [] = error "center of empty list of points"
center xs = Expr $ (show (1.0 / fromIntegral(length xs))) <> fromExpr (foldr1 (+) xs)

infix 4 === , =-= , =|=
(===) :: Expr a -> Expr a -> MP ()
(=-=),(=|=)  :: Expr Pair -> Expr Pair -> MP ()

x === y = out (x <> "=" <> y <> ";\n")
x =-= y = ypart x === ypart y
x =|= y = xpart x === xpart y
                      
out :: Expr a -> MP ()                      
out (Expr x) = MP $ Raw x

