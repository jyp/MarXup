{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MarXup.Tex where

import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Applicative
import Data.Either
import GHC.Exts( IsString(..) )
import System.FilePath
import System.Environment
import Data.List (intersperse)

---------------------------------
-- Marchup interface
textual = Tex  -- TODO: escape \, &, etc.
text = textual 


element :: Tex a -> Tex a
element = id

-----------------------------------
-- Basic datatype and semantics
type Label = Int

data Tex a where
  Return :: a -> Tex a
  Bind :: Tex a -> (a -> Tex b) -> Tex b

  Tex :: String -> Tex () -- ^ raw TeX code

  -- Reference management
  Label :: Tex Label
  Refer :: Label -> Tex Label
  MFix :: (a -> Tex a) -> Tex a 

  -- MetaPOST
  Metapost :: Tex a -> Tex a -- the output should go in the metapost file
  MPFigure :: Label -> Tex () -- inclusion of a metapost figure
  
tex = Tex
type TeX = Tex ()

newLabel = Label  
reference = Refer

instance MonadFix Tex where
  mfix = MFix

instance Monad Tex where
  (>>=) = Bind
  return = Return
  
instance Applicative Tex where
  (<*>) = ap
  pure = Return
  
instance Functor Tex where
  fmap = liftM
  
instance Monoid (Tex ()) where  
  mempty = Tex ""
  mappend = (>>)
  
(<>) :: Monoid a =>  a -> a -> a
(<>) = mappend
  
instance IsString (Tex ()) where  
  fromString = Tex
  
type References = Int -- how many labels have been allocated
emptyRefs :: References
emptyRefs = 0
                  
select :: Bool -> a -> Either a a
select True x = Left x
select False x = Right x
            

type TexAndMPost = [Either String String]

newtype Displayer a = Displayer {fromDisplayer :: RWS () TexAndMPost References a }
  deriving (Monad, MonadWriter TexAndMPost, MonadState References, MonadFix)
-- | Produces a list of strings (either for LaTeX or MetaPOST).
display :: String -> Bool -> Tex a -> Displayer a 
display fname mp t = case t of
      (Tex s) -> tell' s
      (Return a) -> return a
      (Bind k f) -> rec k >>= (rec . f)
      Label -> do x <- get;  put $ x + 1; return x
      (Refer x) -> do tell [select mp $ show x]; return x
      (MFix f) -> mfix (rec . f)
      (Metapost x) -> display fname True x 
      (MPFigure l) -> tell' $ fname ++ "-" ++ show l ++ ".mps"
  where tell' :: String -> Displayer ()
        tell' s = tell [select mp s]   
        rec :: Tex a -> Displayer a
        rec = display fname mp
        
sho :: Show a => a -> Tex ()
sho = Tex . show
                             
render :: Tex a -> IO ()                   
render t = do 
  fname <- getProgName
  let (_,xs) = evalRWS (fromDisplayer $ display fname False t) () emptyRefs 
      (mp,tex) = partitionEithers xs
  writeFile (fname <.> "tex") (concat tex)
  writeFile (fname <.> "mp") (concat mp)
                    

texLn :: String -> Tex ()    
texLn s = Tex s >> Tex "\n"

texLines :: [String] -> Tex ()
texLines = mapM_ texLn

genParen :: String -> Tex a -> Tex a
genParen [l,r] x = Tex [l] *> x <* Tex [r]

braces :: Tex a -> Tex a
braces = genParen "{}"
brackets = genParen "[]"

backslash = Tex ['\\']

nil = braces (Tex "")

cmd0 c = cmdn' c [] [] >> return ()

cmd c = cmd' c []

cmd' cmd options arg = do
  [x] <- cmdn' cmd options [arg]
  return x

cmdn' :: String -> [String] -> [Tex a] -> Tex [a]
cmdn' cmd options args = do 
  backslash >> Tex cmd
  when (not $ null options) $ brackets $ sequence_ $ map tex $ intersperse "," options
  res <- sequence $ map braces args
  when (null args) $ Tex "{}" -- so that this does not get glued with the next thing.
  return res

cmdm :: String -> [Tex a] -> [Tex a] -> Tex [a]
cmdm cmd options args = do 
  backslash >> Tex cmd
  when (not $ null options) $ sequence_ $ map brackets $ options
  res <- sequence $ map braces args
  when (null args) $ Tex "{}" -- so that this does not get glued with the next thing.
  return res


cmdn'_ :: String -> [String] -> [Tex a] -> Tex ()
cmdn'_ cmd options args = cmdn' cmd options args >> return ()

cmdn cmd args = cmdn' cmd [] args
cmdn_ cmd args = cmdn'_ cmd [] args

env :: String -> Tex a -> Tex a
env e body = do
  cmd "begin" $ Tex e
  x <- body
  cmd "end" $ Tex e
  return x

label = do
  l <- Label
  cmd "label" (Refer l)
  
xref l = do
  cmd "ref" (Refer l)
