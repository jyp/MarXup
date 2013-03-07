{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

module MarXup.MultiRef where

import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Applicative
import Data.Either
import GHC.Exts( IsString(..) )
import System.FilePath
import System.Environment
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import System.FilePath

-----------------------------------
-- Basic datatype and semantics
type Label = Int

data Multi a where
  Return :: a -> Multi a
  Bind :: Multi a -> (a -> Multi b) -> Multi b

  Raw :: String -> Multi () -- raw TeX code

  -- Reference management
  Label :: Multi Label -- create a new label
  Refer :: Label -> Multi Label -- a reference, currently yielding exactly the text of the label
  MFix :: (a -> Multi a) -> Multi a -- to be able to refer to future references

  -- Target file management
  Target :: FilePath -> Multi a -> Multi a -- switch to output in another file

instance MonadFix Multi where
  mfix = MFix

instance Monad Multi where
  (>>=) = Bind
  return = Return
  
instance Applicative Multi where
  (<*>) = ap
  pure = Return
  
instance Functor Multi where
  fmap = liftM
  
type References = Int -- how many labels have been allocated
emptyRefs :: References
emptyRefs = 0
            
newtype Outputs = O (Map FilePath [String])

instance Monoid Outputs where
  mempty = O (M.empty)
  mappend (O m) (O n) = O $ M.unionWith (++) m n

f ==> s = O $ M.singleton f [s]
newtype Displayer a = Displayer {fromDisplayer :: RWS FilePath Outputs References a }
  deriving (Monad, MonadWriter Outputs, MonadReader FilePath, MonadState References, MonadFix)

display :: Multi a -> Displayer a 
display t = case t of
      (Raw s) -> tell' s
      (Return a) -> return a
      (Bind k f) -> display k >>= (display . f)
      Label -> do x <- get;  put $ x + 1; return x
      (Refer x) -> do tell' (show x); return x
      (MFix f) -> mfix (display . f)
      (Target f x) -> local (const f) $ display x 
  where tell' :: String -> Displayer ()
        tell' s = do fname <- ask; tell (fname ==> s)

                             
writeToDisk :: Multi a -> IO ()                   
writeToDisk t = do 
  let (_,O xs) = evalRWS (fromDisplayer $ display t) "" emptyRefs 
  forM_ (M.assocs xs) $ \ (fname,contents) ->
    unless (null fname) $
      writeFile fname (concat contents)
 