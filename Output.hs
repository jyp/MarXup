module Output where

import Text.ParserCombinators.Parsek.Position

import Data.Monoid
import Data.DList hiding (foldr, map)

------------------
-- Simple printing combinators, which do not add nor remove line breaks

type Doc = DList Char

text = fromList
x <+> y =  x <> text " " <> y
parens s = singleton '(' <> s <> singleton ')'
braces s = singleton '{' <> s <> singleton '}'
brackets s = singleton '[' <> s <> singleton ']'
doubleQuotes s = singleton '"' <> s <> singleton '"'

int x = text $ show x
hcat :: [Doc] -> Doc
hcat = foldr (<>) mempty
punctuate t = map (<> t)
render :: Doc -> String
render = toList

------------------------------------------
-- Output combinators

oPos :: SourcePos -> Doc
oPos EOF = mempty
oPos p = text "\n{-# LINE" <+> int (sourceLine p) <+> text (show (sourceName p)) <+> text "#-}\n" <>
         Data.DList.replicate (sourceCol p) ' '

oText :: String -> Doc
oText x = text "textual" <+> text (show x)

oConcat :: [Doc] -> Doc
oConcat [] = text "return ()"
oConcat [x] = x
oConcat l = text "do" <+> braces (text "rec" <+> braces (hcat (punctuate (text ";") binds)) <> text ";" <> ret)
  where binds = init l
        ret = last l
