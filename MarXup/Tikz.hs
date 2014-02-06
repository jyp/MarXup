module MarXup.Tikz where

import Control.Monad.LPMonad

newtype Diagram a = Dia (LPT (ReaderT Solution Multi) a)
  deriving (Monad, Applicative, Functor)

