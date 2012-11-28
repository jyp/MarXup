{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module MarchUp.Pandoc (module Data.Monoid,
                       module Text.Pandoc.Definition,
                       Textual (..), Element(..)
                      ) where

import Data.Char
import Text.Pandoc.Definition
import Data.Monoid


class Textual a where
    textual :: String -> a

instance Textual String where
    textual = id

instance Textual [Inline] where
    textual x = [Str x]


instance Textual [Block] where
    textual x = mk [] $ lines x
        where 
              mk []  [] = []
              mk acc [] = [Para acc]
              mk acc (l:ls) | all isSpace l = Para acc : mk [] ls
                            | otherwise = mk (acc ++ [Str l]) ls

class Element a b where
    element :: a -> b

instance Element Block [Block] where
    element = box

instance Element Inline [Inline] where
    element = box

instance Element Inline [Block] where
    element x = [Plain [x]]
    

box x = [x]

{-
-- Inlines
emph = box . Emph
strong = box . Strong
footnote b = box . Note

-- Blocks
header n = box . Header n
-}