{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs, PackageImports #-}

module MarXup.MultiRef where

import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Data.Map.Strict (Map,insert)
import qualified Data.Map.Strict as M
import Graphics.Diagrams.Core (BoxSpec, nilBoxSpec)
type MetaData key = Map key String
type BoxSpecs = Map Int BoxSpec

-- FIXME: Move boxspecs to the Read part.
newtype Multi config key a = Multi {fromMulti :: RWS config String (References,BoxSpecs,MetaData key) a }
  deriving (Functor, Monad, MonadReader config, Applicative, MonadWriter String, MonadState (References,BoxSpecs,MetaData key), MonadFix)

-----------------------------------
-- Basic datatype and semantics
type Label = Int



raw :: String -> Multi config key ()
raw s = tell s

getBoxSpec :: Int -> Multi config key BoxSpec
getBoxSpec bxId = do
  (_,bs,_) <- get
  return $ case M.lookup bxId bs of
    Nothing -> nilBoxSpec -- TODO: log this error somehow
    Just b -> b

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

getMetaData :: Multi config key (MetaData key)
getMetaData = gets (\(_,_,m) -> m)

type References = Int -- how many labels have been allocated
emptyRefs :: References
emptyRefs = 0

