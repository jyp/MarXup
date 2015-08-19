{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, PackageImports #-}

module MarXup.MultiRef where

import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Applicative
import Control.Arrow (first)
import Data.Map.Strict (Map,insert)

type MetaData key = Map key String

newtype Multi config key a = Multi {fromMulti :: RWS config String (References,[BoxSpec],MetaData key) a }
  deriving (Functor, Monad, MonadReader config, Applicative, MonadWriter String, MonadState (References,[BoxSpec],MetaData key), MonadFix)

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

raw :: String -> Multi config key ()
raw s = tell s

getBoxSpec :: Multi config key BoxSpec
getBoxSpec = do
  (refs,bs,m) <- get
  case bs of
    [] -> error "display: ran out of boxes!"
    (b:bs') -> do
      put (refs,bs',m)
      return b

-- | allocate a new label
newLabel :: Multi key config Label -- create a new label
newLabel = do (r,bx,m) <- get
              put (r+1,bx,m)
              return r

-- | output some meta data
metaData :: Ord key => key -> String -> Multi config key ()
metaData k val = do
   (r,bx,m) <- get
   put (r,bx,insert k val m)
   return ()

type References = Int -- how many labels have been allocated
emptyRefs :: References
emptyRefs = 0

