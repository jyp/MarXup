module MarchUp.SimpleText (module Data.Monoid, element, textual) where

import qualified MarchUp.Text as T

import Data.Monoid
import Data.Traversable
import Data.Foldable
import Control.Applicative

type Text = T.Text String

element :: Show a => a -> Text
element = T.Elem . show

textual = T.Text

instance Show Text where
    showsPrec p = T.linearize showString . fmap showString

