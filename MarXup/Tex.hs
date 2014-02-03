{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies,TypeSynonymInstances,FlexibleInstances #-}

module MarXup.Tex where

import MarXup
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Applicative
import GHC.Exts( IsString(..) )
import System.FilePath
import System.Environment
import Data.List (intersperse)
import MarXup.MultiRef
import Data.Monoid
import Graphics.DVI
import System.Process

data MPOutFormat = SVG | EPS
  deriving (Eq,Show)

newtype Tex a = Tex {fromTex :: ReaderT (FilePath,MPOutFormat) Multi a}
  deriving (Monad, MonadFix, Applicative, Functor, MonadReader (FilePath,MPOutFormat))

---------------------------------
-- MarXup interface
textual :: String -> TeX
textual s = Tex $ lift (Raw $ concatMap escape s)

kern :: String -> TeX
kern x = braces $ tex $ "\\kern " ++ x

escape '\\' = "\\ensuremath{\\backslash{}}"
escape '~' = "\\ensuremath{\\sim{}}"
escape '<' = "\\ensuremath{<}"
escape '>' = "\\ensuremath{>}"
escape '_' = "\\_"
escape c | c `elem` "{}&$" = '\\':c:[]
escape c = [c]

instance Element (Tex a) where
  type Target (Tex a) = Tex a
  element = id

tex :: String -> TeX
tex = Tex . lift . Raw
type TeX = Tex ()

newLabel :: Tex Label
newLabel = Tex $ lift Label
reference = Tex . lift . Refer

instance Monoid (TeX) where
  mempty = textual ""
  mappend = (>>)

instance IsString (TeX) where
  fromString = textual

renderToDisk :: MPOutFormat -> Tex a -> IO ()
renderToDisk fmt t = do
  fname <- getProgName
  renderToDisk' fmt fname t

renderToDisk' :: MPOutFormat -> String -> Tex a -> IO ()
renderToDisk' fmt fname (Tex t) = 
  writeToDisk (Target (fname <.> "tex") $ runReaderT t (fname,fmt))

getMpOutFormat :: Tex MPOutFormat
getMpOutFormat = snd <$> ask

getOutFile :: Tex FilePath
getOutFile = fst <$> ask


render :: Tex a -> [String]
render (Tex t) = renderMainTarget (runReaderT t ("<interactive>",EPS))

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

cmdn' :: String -> [String] -> [Tex a] -> Tex [a]
cmdn' cmd options args = do
  backslash >> tex cmd
  when (not $ null options) $ brackets $ sequence_ $ map tex $ intersperse "," options
  res <- sequence $ map braces args
  when (null args) $ tex "{}" -- so that this does not get glued with the next thing.
  return res

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
env' e opts body = do
  cmd "begin" $ tex e
  when (not $ null opts) $ brackets $ sequence_ $ map tex $ intersperse "," opts
  x <- body
  cmd "end" $ tex e
  return x

-- | Environment with a tex option
env'' :: String -> TeX -> Tex a -> Tex a
env'' e opts body = do
  cmd "begin" $ tex e
  brackets opts
  x <- body
  cmd "end" $ tex e
  return x

------------------
-- Sorted labels

data SortedLabel =  SortedLabel String Label

label :: String -> Tex SortedLabel
label s = do
  l <- newLabel
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

generateBoxes :: Tex a -> String
generateBoxes (Tex t) = mconcat (map inShipout bxs) 
  where (_,_,Boxes bxs _) = runRWS (fromBoxer $ getBoxes $ runReaderT t ("<no filepath>",EPS) ) False 0
        inShipout x = "\\mpxshipout%\n" ++ x ++ "\n\\stopmpxshipout\n"

shipoutMacros = tex "\
\  \\gdef\\mpxshipout{\\shipout\\hbox\\bgroup                              \n\
\    \\setbox0=\\hbox\\bgroup}                                             \n\
\  \\gdef\\stopmpxshipout{\\egroup  \\dimen0=\\ht0 \\advance\\dimen0\\dp0  \n\
\    \\dimen1=\\ht0 \\dimen2=\\dp0                                         \n\
\    \\setbox0=\\hbox\\bgroup                                              \n\
\      \\box0                                                              \n\
\      \\ifnum\\dimen0>0 \\vrule width1sp height\\dimen1 depth\\dimen2     \n\
\      \\else \\vrule width1sp height1sp depth0sp\\relax                   \n\
\      \\fi\\egroup                                                        \n\
\    \\ht0=0pt \\dp0=0pt \\box0 \\egroup}                                  \n\
\ "

renderWithBoxes :: [BoxSpec] -> Tex a -> String
renderWithBoxes bs (Tex t) = doc
  where (_,_,doc) = runRWS (fromDisplay'er $ display' $ runReaderT t ("<no filepath>",EPS) ) () (0,bs)

renderTex :: (TeX -> TeX) -> TeX -> IO String
renderTex salamalecs t = do
  let bxsTex = generateBoxes (salamalecs $ shipoutMacros >> t)
      boxesName = "mpboxes"
  writeFile (boxesName ++ ".tex") bxsTex
  system $ "latex " ++ boxesName
  boxes <- withDVI (boxesName ++ ".dvi") (\_ _ -> return emptyFont) () getBoxInfo
  return $ renderWithBoxes boxes $ salamalecs t

getBoxInfo :: () -> Page -> IO (Maybe ((), BoxSpec))
getBoxInfo () (Page _ [(_,Graphics.DVI.Box objs)] _) = return (Just ((),dims))
  where ((width,descent),Rule _ ascent) = last objs
        dims = BoxSpec (fromIntegral width) (fromIntegral ascent) (fromIntegral descent)

