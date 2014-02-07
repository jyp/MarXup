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
data BoxSpec = BoxSpec {boxWidth, boxAscent, boxDescent :: Double}
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
  Refer :: Label -> Multi Label -- a reference, currently yielding exactly the text of the label TODO: remove this.
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

-- | Interpret to write into a map from filename to contents.
newtype Displayer a = Displayer {fromDisplayer :: RWS FilePath Outputs References a }
  deriving (Monad, MonadWriter Outputs, MonadReader FilePath, MonadState References, MonadFix)

display :: Multi a -> Displayer a
display t = case t of
      (Raw _ s) -> tell' s
      (Return a) -> return a
      (Bind k f) -> display k >>= (display . f)
      Label -> do x <- get;  put $ x + 1; return x
      (Refer x) -> do tell' (show x); return x
      (MFix f) -> mfix (display . f)
      (Target f x) -> local (const f) $ display x 
  where tell' :: String -> Displayer ()
        tell' s = do fname <- ask; tell (fname ==> s)
        f ==> s = O $ M.singleton f [s]

-- data Selection = Sel {selJustBoxes :: Bool, selRegular :: Bool}
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
      (Refer x) -> do tell (show x); return x
      (MFix f) -> mfix (display' . f)
      (Target _ x) -> display' x
      (Box x) -> do
        a <- local moveInsideBox $ display' x
        (b:_) <- snd <$> get
        return (a,b)

-- data Boxes = Boxes {completeBoxes :: [String], currentBox :: [String]}
-- finishBox :: Boxes -> Boxes
-- finishBox (Boxes bxs bx) = Boxes (mconcat bx:bxs) []
-- instance Monoid Boxes where
--   mappend (Boxes bxs bx) (Boxes axs ax) = Boxes (bxs ++ axs) (bx ++ ax)
--   mempty = Boxes [] []
-- newtype Boxer a = Boxer {fromBoxer :: RWS Bool Boxes References a }
--   deriving (Monad, MonadWriter Boxes, MonadReader Bool, MonadState References, MonadFix)

-- getBoxes :: Multi a -> Boxer a
-- getBoxes t = case t of
--       (Raw s) -> tell' s
--       (Return a) -> return a
--       (Bind k f) -> getBoxes k >>= (getBoxes . f)
--       Label -> do x <- get; put $ x + 1; return x
--       (Refer x) -> do tell' (show x); return x
--       (MFix f) -> mfix (getBoxes . f)
--       (Target _f x) -> getBoxes x
--       (Box x) -> do
--         inBox <- ask
--         when inBox $ error "nested boxes not supported!"
--         bx <- censor finishBox $ local (\_ -> True) $ getBoxes x
--         return (bx,nilBoxSpec)
--   where tell' :: String -> Boxer ()
--         tell' s = do
--           inBox <- ask
--           when inBox $ tell (Boxes [] [s])


-- | Write the relevant files to disk
writeToDisk :: Multi a -> IO ()
writeToDisk t = do 
  let (_,O xs) = evalRWS (fromDisplayer $ display t) "" emptyRefs 
  forM_ (M.assocs xs) $ \ (fname,contents) ->
    unless (null fname) $
      writeFile fname (concat contents)

renderMainTarget :: Multi a -> [String]
renderMainTarget t = M.findWithDefault [] "" xs
  where (_,O xs) = evalRWS (fromDisplayer $ display t) "" emptyRefs 
