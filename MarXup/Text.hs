module MarXup.Text (Text(..), textual, element, module Data.Monoid, linearize) where

import Data.Monoid
import Data.Traversable
import Data.Foldable
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Writer

data Text a where
  Return :: a -> Text a 
  Text :: String -> Text () 
  (:>>=) :: Text a -> (a -> Text b) -> Text b 
  MFix :: (a -> Text a) -> Text a

textual = Text

element :: Show a => a -> Text ()
element = Text . show

instance MonadFix Text where
  mfix = MFix

instance Monad Text where
  (>>=) = (:>>=)
  return = Return
  
instance Applicative Text where
  (<*>) = ap
  pure = Return
  
instance Functor Text where
  fmap = liftM

run :: Text a -> Writer [String] a
run (Return x) = return x
run (Text x) = tell [x]
run (k :>>= f) = run k >>= run . f
run (MFix f) = mfix (run . f)

linearize :: Text a -> [String]
linearize t = snd $ runWriter $ run t