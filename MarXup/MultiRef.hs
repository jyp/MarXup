{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, PackageImports #-}

module MarXup.MultiRef where

import Control.Monad.Fix
import "mtl" Control.Monad.RWS.Lazy
import Control.Applicative
import Control.Arrow (first)

newtype Multi a = Multi {fromMulti :: RWS () String (References,[BoxSpec]) a }
  deriving (Functor, Monad, Applicative, MonadWriter String, MonadState (References,[BoxSpec]), MonadFix)

-----------------------------------
-- Basic datatype and semantics
type Label = Int

-- | Size of a box, in points. boxDepth is how far the baseline is
-- from the bottom. boxHeight is how far the baseline is from the top.
-- (These are TeX meanings)
data BoxSpec = BoxSpec {boxWidth, boxHeight, boxDepth :: Double}
             deriving (Show)

nilBoxSpec :: BoxSpec
nilBoxSpec = BoxSpec 0 0 0

raw :: String -> Multi ()
raw s = tell s

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

