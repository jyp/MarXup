{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

module MarXup.MultiRef where

import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Applicative
import Control.Arrow (first)

newtype Multi a = Multi {fromMulti :: RWS InterpretMode String (References,[BoxSpec]) a }
  deriving (Functor, Monad, Applicative, MonadWriter String, MonadState (References,[BoxSpec]), MonadFix, MonadReader InterpretMode)

-----------------------------------
-- Basic datatype and semantics
type Label = Int

-- | Size of a box, in points. boxDescent is how far the baseline is
-- from the bottom.
data BoxSpec = BoxSpec {boxWidth, boxHeight, boxDescent :: Double}
             deriving (Show)

nilBoxSpec :: BoxSpec
nilBoxSpec = BoxSpec 0 0 0

raw :: Mode -> String -> Multi ()
raw mode s = do
  interpretMode <- ask
  when (mode interpretMode) $ tell s

getBoxSpec :: Multi BoxSpec
getBoxSpec = do
  (refs,bs) <- get
  case bs of
    [] -> error "display: ran out of boxes!"
    (b:bs') -> do
      put (refs,bs')
      return b
 


-- Reference management
newLabel :: Multi Label -- create a new label
newLabel = do x <- fst <$> get;  modify (first (+1)); return x

type References = Int -- how many labels have been allocated
emptyRefs :: References
emptyRefs = 0

type Mode = InterpretMode -> Bool
data InterpretMode = OutsideBox | InsideBox | Regular deriving Eq

