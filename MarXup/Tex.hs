{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies,TypeSynonymInstances,FlexibleInstances, PackageImports #-}

module MarXup.Tex where

import MarXup
import Control.Monad.Reader
import Control.Monad.RWS
import GHC.Exts( IsString(..) )
import Data.List (intersperse,intercalate)
import MarXup.MultiRef
import System.Directory (doesFileExist)
import Data.Char (isSpace,toLower)
import Data.Map (assocs, Map)
import qualified Data.Map as Map
import Graphics.Diagrams.Core (BoxSpec (..))

data ClassFile = ACMArt | Plain | LNCS | SIGPlan | IEEE | EPTCS | Beamer
  deriving (Ord,Eq,Show)


------------------------------------
-- MetaData

data Key = PreClass ClassFile | PrePackage Int {- priority -} String | PreTheorem String String
  deriving (Ord,Eq)

newtheorem :: String -> String -> TeX
newtheorem ident txt = do
  sty <- askClass
  unless ((sty == LNCS || sty == Beamer || sty == ACMArt) && ident `elem` ["theorem", "corollary", "lemma", "definition", "proposition"]) $ do
  Tex $ metaData (PreTheorem ident txt) ""

usepkg :: String -> Int -> [String] -> TeX
usepkg ident prio options = Tex $ metaData (PrePackage prio ident) (intercalate "," options)

documentClass :: ClassFile -> [String] -> TeX
documentClass docClass options = Tex $ metaData (PreClass docClass) (intercalate "," options)

className :: ClassFile -> String
className = \case
  Plain -> "article"
  SIGPlan -> "sigplanconf"
  c -> map toLower (show c)

renderKey :: Key -> String -> String
renderKey o options = case o of
  PreClass name -> "\\documentclass[" ++ options ++ "]{" ++ className name ++ "}"
  PrePackage _ name -> "\\usepackage[" ++ options ++ "]{" ++ name ++ "}"
  PreTheorem ident txt  -> "\\newtheorem{" ++ ident ++ "}{" ++ txt ++ "}"

newtype Tex a = Tex {fromTex :: Multi () Key a}
  deriving (Monad, MonadFix, Applicative, Functor)

instance MonadFail Tex where
  fail = error



---------------------------------
-- MarXup interface
instance Textual Tex where
  textual s = case break (== '\n') s of
    -- The 1st blank line of a MarXup chunk is replaced by a
    -- space. This means that to create a paragraph after an element,
    -- one needs a double blank line.
    (l,'\n':s') | all isSpace l -> tex (' ' : process s')
    _ -> tex $ process s
   where process = concatMap escape

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

texComment :: String -> TeX
texComment s =
  forM_ (lines s) $ \line ->
    tex $ "% " <> line <> "\n"

type TeX = Tex ()

reference :: Label -> Tex ()
reference l = tex (show l)

instance Monoid (TeX) where
  mempty = tex ""

instance Semigroup (TeX) where
  (<>) = (>>)

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
  xs <- cmdn' cmd options [arg]
  case xs of
    [x] -> return x
    _ -> error "MarXup.Tex: cmd': the impossible happened"

-- | Command with options and many arguments
cmdn' :: String -> [String] -> [Tex a] -> Tex [a]
cmdn' cmd options args = do
  backslash >> tex cmd
  when (not $ null options) $ brackets $ sequence_ $ map tex $ intersperse "," options
  res <- sequence $ map braces args
  when (null args) $ tex "{}" -- so that this does not get glued with the next thing.
  return res

-- | Command with tex options and several arguments
cmdm :: String -> [Tex a] -> [Tex a] -> Tex [a]
cmdm cmd options args = do
  backslash >> tex cmd
  when (not $ null options) $ sequence_ $ map brackets $ options
  res <- sequence $ map braces args
  when (null args) $ tex "{}" -- so that this does not get glued with the next thing.
  return res


-- | Command with string options and several arguments; no result
cmdn'_ :: String -> [String] -> [TeX] -> Tex ()
cmdn'_ cmd options args = cmdn' cmd options args >> return ()

-- | Command with n arguments
cmdn :: String -> [Tex a] -> Tex [a]
cmdn c args = cmdn' c [] args

-- | Command with n arguments, no result
cmdn_ :: String -> [TeX] -> Tex ()
cmdn_ theCmd args = cmdn'_ theCmd [] args

-- | Environment
env :: String -> Tex a -> Tex a
env x = env' x []

-- | Environment with options
env' :: String -> [String] -> Tex a -> Tex a
env' e opts body = env'' e (map textual opts) [] body

-- | Environment with tex options and tex arguments
env'' :: String -> [TeX] -> [TeX] -> Tex a -> Tex a
env'' e opts args body = do
  cmd "begin" $ tex e
  when (not $ null opts) $ brackets $ sequence_ $ intersperse (tex ",") opts
  mapM_ braces args
  x <- body
  cmd "end" $ tex e
  return x

------------------
-- Sorted labels

data SortedLabel = SortedLabel String Label

label :: String -> Tex SortedLabel
label s = do
  l <- Tex newLabel
  cmd "label" (reference l)
  return $ SortedLabel s l

xref :: SortedLabel -> TeX
xref l = xrefWith (cmd "ref") [l]

xrefWith :: (TeX -> Tex a) -> [SortedLabel] -> TeX
xrefWith c ls = do
  _ <- c (tex (intercalate "," [show l | SortedLabel _ l <- ls]))
  return ()

usepkgCleverRef :: TeX
usepkgCleverRef = usepkg "cleveref" 1000 []

cleverefCmd :: Tex b -> Tex b
cleverefCmd r = do
  usepkgCleverRef
  cmd "cref" r

crefCapCmd :: Tex b -> Tex b
crefCapCmd r = do
  usepkgCleverRef
  cmd "Cref" r

cref :: SortedLabel -> TeX
cref l = crefs [l]

crefs :: [SortedLabel] -> TeX
crefs = xrefWith cleverefCmd

ccrefs :: [SortedLabel] -> TeX
ccrefs = xrefWith crefCapCmd

pageref :: SortedLabel -> TeX
pageref (SortedLabel _ l) = do
  cmd "pageref" (reference l)
  return ()

instance Element SortedLabel where
  type Target SortedLabel = TeX
  element x = cref x >> return ()

-----------------
-- Generate boxes


inBox :: Tex a -> Tex (a, BoxSpec)
inBox = helpBox True

justBox :: Tex a -> Tex BoxSpec
justBox x = snd <$> helpBox False x


helpBox :: Bool -> Tex a -> Tex (a,BoxSpec)
helpBox showBox x = do bxId <- Tex newLabel
                       a <- fillBox bxId showBox x
                       b <- Tex (getBoxSpec bxId)
                       return (a,b)

getBoxFromId :: Int -> Tex BoxSpec
getBoxFromId = Tex . getBoxSpec

fillBox :: Label -> Bool -> Tex a -> Tex a
fillBox bxId showBox x = braces $ do
  tex $ "\\savebox{\\marxupbox}{"
  a <- x
  tex $
    "}"
    ++ "\\immediate\\write\\boxesfile{" ++ show bxId ++ "}"
    ++ writeBox "wd"
    ++ writeBox "ht"
    ++ writeBox "dp"
  when showBox $ tex $ "\\box\\marxupbox"

  return a
  where writeBox l = "\\immediate\\write\\boxesfile{\\number\\"++ l ++"\\marxupbox}"

-- TODO: it would be nice to have:
-- withBox :: Bool -> Tex a -> (Tex a -> Tex a) -> Tex BoxSpec
-- withBox showBox boxContents boxContext = ...

-- however this may require some latex trickery. The idea would be to
-- create the box upfront (and thus we can obtain its boundaries), and
-- invoke it wherever the context calls it. This means that we need a
-- new 'box' for every invokation. It may be that latex does not have
-- infinitely many boxes, and thus we need to reuse (latex-side) box
-- ids.

renderWithBoxes :: BoxSpecs -> Tex a -> String
renderWithBoxes bs (Tex t) = (preamble ++ doc)
  where (_,(_,_,metaDatum),doc) = runRWS (fromMulti $ t) () (0,bs,mempty)
        preamble :: String
        preamble = unlines $ map (uncurry renderKey) $ assocs metaDatum

renderSimple :: Tex a -> String
renderSimple = renderWithBoxes Map.empty

renderTex :: String -> TeX -> IO ()
renderTex fname body = do
  let boxesTxt = fname ++ ".boxes"
  boxes <- getBoxInfo . map read . lines <$> do
    e <- doesFileExist boxesTxt
    if e
      then readFile boxesTxt
      else return ""
  putStrLn $ "Found " ++ show (length boxes) ++ " boxes"
  let texSource = renderWithBoxes boxes wholeDoc
      wholeDoc = do
        tex $ "\\newwrite\\boxesfile"
        tex $ "\\immediate\\openout\\boxesfile="++boxesTxt++"\n\\newsavebox{\\marxupbox}"
        body
        tex "\n\\immediate\\closeout\\boxesfile"
  writeFile (fname ++ ".tex") texSource

askClass :: Tex ClassFile
askClass = Tex $ do
  k <- Map.keys <$> getMetaData
  case [c | PreClass c <- k] of
    [] -> error "class not specified (use documentClass command earlier)"
    [c] -> return c
    _ ->  error "do not use documentclass more than once"

getBoxInfo :: [Int] -> Map Int BoxSpec
getBoxInfo (ident:width:height:depth:bs) = Map.insert ident (BoxSpec (scale width) (scale height) (scale depth)) (getBoxInfo bs)
  where scale x = fromIntegral x / 65536
getBoxInfo _ = Map.empty

