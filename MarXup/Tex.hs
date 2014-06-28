{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies,TypeSynonymInstances,FlexibleInstances, PackageImports #-}

module MarXup.Tex where

import MarXup
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.RWS
import Control.Applicative
import GHC.Exts( IsString(..) )
import Data.List (intersperse)
import MarXup.MultiRef
import System.Process
import System.Directory (doesFileExist)

newtype Tex a = Tex {fromTex :: Multi a}
  deriving (Monad, MonadFix, Applicative, Functor)

---------------------------------
-- MarXup interface
instance Textual Tex where
    textual s = tex $ concatMap escape s

kern :: String -> TeX
kern x = braces $ tex $ "\\kern " ++ x

escape :: Char -> [Char]
escape '\\' = "\\ensuremath{\\backslash{}}"
escape '~' = "\\ensuremath{\\sim{}}"
escape '<' = "\\ensuremath{<}"
escape '>' = "\\ensuremath{>}"
escape c | c `elem` "#^_{}&$%" = '\\':c:[]
escape c = [c]

instance Element (Tex a) where
  type Target (Tex a) = Tex a
  element = id

tex ::  String ->TeX
tex = Tex . raw

type TeX = Tex ()

reference :: Label -> Tex ()
reference l = tex (show l)

instance Monoid (TeX) where
  mempty = textual ""
  mappend = (>>)

instance IsString (TeX) where
  fromString = textual

texLn :: String -> TeX
texLn s = tex s >> tex "\n"

texLines :: [String] -> Tex ()
texLines = mapM_ texLn

genParen :: String -> Tex a -> Tex a
genParen [l,r] x = tex [l] *> x <* tex [r]

braces,brackets :: Tex a -> Tex a
braces = genParen "{}"
brackets = genParen "[]"

backslash :: TeX
backslash = tex ['\\']

nil :: TeX
nil = braces (tex "")

-- | Command with no argument
cmd0 :: String -> Tex ()
cmd0 c = cmdn' c [] [] >> return ()

-- | Command with one argument
cmd :: String -> Tex a -> Tex a
cmd c = cmd' c []

-- | Command with options
cmd' :: String -> [String] -> Tex b -> Tex b
cmd' cmd options arg = do
  [x] <- cmdn' cmd options [arg]
  return x

-- | Command with options and many arguments
cmdn' :: String -> [String] -> [Tex a] -> Tex [a]
cmdn' cmd options args = do
  backslash >> tex cmd
  when (not $ null options) $ brackets $ sequence_ $ map tex $ intersperse "," options
  res <- sequence $ map braces args
  when (null args) $ tex "{}" -- so that this does not get glued with the next thing.
  return res

-- | Command with tex options and many arguments
cmdm :: String -> [Tex a] -> [Tex a] -> Tex [a]
cmdm cmd options args = do
  backslash >> tex cmd
  when (not $ null options) $ sequence_ $ map brackets $ options
  res <- sequence $ map braces args
  when (null args) $ tex "{}" -- so that this does not get glued with the next thing.
  return res


cmdn'_ :: String -> [String] -> [TeX] -> Tex ()
cmdn'_ cmd options args = cmdn' cmd options args >> return ()

-- | Command with n arguments
cmdn :: String -> [Tex a] -> Tex [a]
cmdn c args = cmdn' c [] args

cmdn_ :: String -> [TeX] -> Tex ()
cmdn_ cmd args = cmdn'_ cmd [] args

-- | Environment
env :: String -> Tex a -> Tex a
env x = env' x []

-- | Environment with options
env' :: String -> [String] -> Tex a -> Tex a
env' e opts body = env'' e opts [] body

-- | Environment with a tex option
env'' :: String -> [String] -> [TeX] -> Tex a -> Tex a
env'' e opts args body = do
  cmd "begin" $ tex e
  when (not $ null opts) $ brackets $ sequence_ $ map tex $ intersperse "," opts
  mapM_ braces args
  x <- body
  cmd "end" $ tex e
  return x

------------------
-- Sorted labels

data SortedLabel =  SortedLabel String Label

label :: String -> Tex SortedLabel
label s = do
  l <- Tex newLabel
  cmd "label" (reference l)
  return $ SortedLabel s l

xref :: SortedLabel -> TeX
xref (SortedLabel _ l) = do
  cmd "ref" (reference l)
  return ()

fxref :: SortedLabel -> TeX
fxref l@(SortedLabel s _) = do
  textual s
  tex "~" -- non-breakable space here
  xref l

instance Element SortedLabel where
  type Target SortedLabel = TeX
  element x = fxref x >> return ()


-----------------
-- Generate boxes



inBox :: Tex a -> Tex (a, BoxSpec)
inBox x = braces $ do
  tex $ "\\savebox{\\marxupbox}{"
  a <- x
  tex $
    "}"
    ++ writeBox "wd"
    ++ writeBox "ht"
    ++ writeBox "dp"
  tex $ "\\box\\marxupbox"
  b <- Tex getBoxSpec

  return (a,b)
  where writeBox l = "\\immediate\\write\\boxesfile{\\number\\"++ l ++"\\marxupbox}"

renderWithBoxes :: [BoxSpec] -> Tex a -> String
renderWithBoxes bs (Tex t) = doc
  where (_,_,doc) = runRWS (fromMulti $ t) () (0,bs)

renderSimple :: TeX -> String
renderSimple = renderWithBoxes []
  
renderTex :: TeX -> IO String
renderTex body = do
  let bxsTex = renderWithBoxes (repeat nilBoxSpec) wholeDoc
      boxesName = "mpboxes"
      boxesTxt = boxesName ++ ".txt"
      wholeDoc = do
        tex $ "\\newwrite\\boxesfile"
        tex $ "\\immediate\\openout\\boxesfile="++boxesTxt++"\n\\newsavebox{\\marxupbox}"
        body
        tex "\n\\immediate\\closeout\\boxesfile"
  writeFile (boxesName ++ ".tex") bxsTex
  system $ "latex " ++ boxesName
  boxes <- do
    e <- doesFileExist boxesTxt
    if e
      then do
        boxData <- map read . lines <$> readFile boxesTxt
        return $ getBoxInfo boxData
      else return []
  putStrLn $ "Number of boxes found: " ++ show (length boxes)
  return $ renderWithBoxes boxes $ wholeDoc

getBoxInfo :: [Int] -> [BoxSpec]
getBoxInfo [] = []
getBoxInfo (width:height:depth:bs) = BoxSpec (scale width) (scale height) (scale depth):getBoxInfo bs
  where scale x = fromIntegral x / 65536

