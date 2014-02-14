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
import Control.Arrow (first,second)

-----------------------------------
-- Basic datatype and semantics
type Label = Int
data BoxSpec = BoxSpec {boxWidth, boxHeight, boxDescent :: Double}
               -- unit is (probably) the point
             deriving (Show)
nilBoxSpec = BoxSpec 0 0 0

data Multi a where
  Return :: a -> Multi a
  Bind :: Multi a -> (a -> Multi b) -> Multi b

  Raw :: Mode -> String -> Multi () -- raw TeX code
  Box :: Multi a -> Multi (a, BoxSpec)

  -- Reference management
  Label :: Multi Label -- create a new label
  MFix :: (a -> Multi a) -> Multi a -- to be able to refer to future references

  -- Target file management
  Target :: FilePath -> Multi a -> Multi a -- locally switch to output in another file

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

data Mode = Normal | BoxOnly | NotBoxOnly | Always
data InterpretMode = OutsideBox | InsideBox | Regular deriving Eq

moveInsideBox OutsideBox = InsideBox
moveInsideBox x = x

shouldShow :: Mode -> InterpretMode -> Bool
shouldShow Always _ = True
shouldShow Normal Regular = True
shouldShow Normal OutsideBox = False
shouldShow Normal InsideBox = True
shouldShow BoxOnly Regular = False
shouldShow BoxOnly OutsideBox = True
shouldShow BoxOnly InsideBox = True
shouldShow NotBoxOnly Regular = True
shouldShow NotBoxOnly _ = False
  
-- | Interpret to write into a map from filename to contents.
newtype Display'er a = Display'er {fromDisplay'er :: RWS InterpretMode String (References,[BoxSpec]) a }
  deriving (Functor, Monad, MonadWriter String, MonadState (References,[BoxSpec]), MonadFix, MonadReader InterpretMode)

display' :: Multi a -> Display'er a
display' t = case t of
      (Raw mode s) -> do
          interpretMode <- ask
          when (shouldShow mode interpretMode) $ tell s
      (Return a) -> return a
      (Bind k f) -> display' k >>= (display' . f)
      Label -> do x <- fst <$> get;  modify (first (+1)); return x
      (MFix f) -> mfix (display' . f)
      (Target _ x) -> display' x
      (Box x) -> do
        (refs,bs) <- get
        b <- case bs of
          [] -> error "display': ran out of boxes!"
          (b:bs') -> do
            put (refs,bs')
            return b
        a <- local moveInsideBox $ display' x
        return (a,b)
