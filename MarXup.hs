{-# LANGUAGE TypeFamilies #-}

module MarXup where

class Element a where
  type Target a
  element :: a -> Target a


