{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, EmptyDataDecls #-}

module MarXup.MetaPost where

import MarXup.Tex
import MarXup.MultiRef
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative
import GHC.Exts( IsString(..) )
import Data.Monoid
import System.FilePath
import Data.Ratio  
import Data.Char (toLower)

newtype MP a = MP (WriterT [MP ()] Multi a)
  deriving (Monad, MonadFix, Applicative, Functor, MonadWriter [MP ()])

instance IsString (MP ()) where
   fromString = mpRaw

instance Monoid (MP()) where  
  mempty = return ()
  mappend = (>>)


mpRaw :: String -> MP ()
mpRaw x = MP $ lift $ Raw x

mpRawLines :: [String] -> MP ()
mpRawLines ls = mpRaw $ concat $ map (++"\n") ls 

mpRefer l = MP (lift $ Refer l) >> return ()
mpLabel  = MP $ lift $ Label

sho :: Show a => a -> MP ()
sho = mpRaw . show

mpRawTex :: Tex a -> MP a
mpRawTex (Tex t) = MP $ lift (runReaderT t ("<in metapost>",EPS))

fmtExt fmt = case fmt of
        SVG -> "svg"
        EPS -> "mps"

metaPostPreamble :: MPOutFormat -> Tex a -> MP ()
metaPostPreamble fmt texPreamble =  do
  mpRawLines ["outputtemplate := \"%j-%c."++fmtExt fmt++"\";",
              "outputformat := " ++ show (show fmt) ++ ";",
              "verbatimtex%&latex",
              ""]
  mpRawTex texPreamble
  mpRawLines [""
             ,"\\begin{document}"
             ,"etex;"
             ,"prologues:=3;"
             ,"input drv;"
             ] 
metaPostEpilogue = mpRawLines ["end"]
    

mkfig :: Label -> MP () -> MP ()
mkfig lab t = do
  mpRaw "beginfig(" >> mpRefer lab >> mpRaw ")\n"
  execDelayed t
  mpRaw "endfig;\n"

delay :: MP () -> MP ()
delay x = tell [x]

runDelayed :: MP a -> Multi a
runDelayed (MP x) = do
  (x,delayed) <- runWriterT x
  mapM_ runDelayed delayed
  return x


execDelayed :: MP a -> MP a 
execDelayed x = MP $ lift $ runDelayed x


inMP :: MP a -> Tex a
inMP mp = do
 fname <- getOutFile
 Tex $ lift $ Target (fname <.> "mp") $ runDelayed mp

mpQuote :: TeX -> MP ()
mpQuote t = "\"" <> mpRawTex t <> "\""

mpTex :: TeX -> MP ()
mpTex t = "btex " <> mpRawTex t <> " etex"

createMetaPostFigure :: Label -> MP () -> Tex ()
createMetaPostFigure lab t = do 
  inMP $ mkfig lab t

includeMetaPostFigure :: [String] -> Label -> TeX
includeMetaPostFigure opts l = do
  (fname,format) <- ask 
  case format of
    EPS -> let figname = fname ++ "-" ++ show l ++ ".mps"
           in  cmd' "includegraphics" opts (tex figname)
    SVG -> do let figname = fname ++ "-" ++ show l
              tex "\\def\\svgwidth{\\columnwidth}"
              cmd' "includesvg" opts (tex figname)

-- | Create and include a metapost drawing in one go. The 1st argument
-- are the options to includegraphics
mpFigure :: [String] -> MP () -> Tex Label
mpFigure opts mp = do
  l <- newLabel
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
data Path
data DrawOption
data DashPattern

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
out (Expr x) = mpRaw x

if_ :: Expr Bool -> MP () -> MP ()
if_ cond bod = "if " <> out cond <> ":" <> bod <> "fi;\n"

if_else :: Expr Bool -> MP () -> MP () -> MP ()
if_else cond then_ else_ = "if " <> out cond <> ":" <> then_ <> "\nelse: " <> else_ <>  "fi;\n"

defaultVal expr v = if_ (unknown expr) (expr === v)

unknown :: Expr Numeric -> Expr Bool
unknown (Expr x) = Expr $ "unknown " <> x

(.>.) :: Expr Numeric -> Expr Numeric -> Expr Bool
(Expr x) .>. (Expr y) = Expr (x <> ">" <> y)
          
infixr 4 ...,....,.--,.!

closed :: Expr Path
closed = Expr "cycle"

(.!) :: Expr Pair -> Expr Path
(.!) (Expr x) = Expr x

(.--),(...),(....) :: Expr Pair -> Expr Path -> Expr Path
Expr x .-- Expr y = Expr $ x <> "--" <> y
Expr x ... Expr y = Expr $ x <> ".." <> y
Expr x .... Expr y = Expr $ x <> "..." <> y

dashed :: Expr DashPattern -> Expr DrawOption
dashed (Expr x) = Expr ("dashed " <> x)

evenly :: Expr DashPattern
evenly = Expr "evenly"

draw :: Expr Path -> [Expr DrawOption] -> MP ()
draw path opts = "draw " <> out path <> mconcat [" " <> out o | o <- opts] <> ";\n"





