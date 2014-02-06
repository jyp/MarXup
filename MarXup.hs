{-# LANGUAGE TypeFamilies #-}

module MarXup where

class Element a where
  type Target a
  element :: a -> Target a

class Textual f where
    textual :: String -> f ()
