import Text.ParserCombinators.Parsek.Position

import System.Environment
import Data.Monoid
import Data.DList hiding (foldr, map)
import MarXupParser

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
oPos p = text "\n{-# LINE" <+> int (sourceLine p) <+> text (show (sourceName p)) <+> text "#-}\n"

oText :: String -> Doc
oText x = text "textual" <+> text (show x)

oConcat :: [Doc] -> Doc
oConcat [] = text "return ()"
oConcat [x] = x
oConcat l = text "do" <+> braces (text "rec" <+> braces (hcat (punctuate (text ";") binds)) <> text ";" <> ret)
  where binds = init l
        ret = last l

----------------------------------------------
-- Top-level generation

rHaskells :: [Haskell] -> Doc
rHaskells xs = mconcat $ map rHaskell xs

rHaskell :: Haskell -> DList Char
rHaskell (HaskChunk s) = text s
rHaskell (HaskLn pos) = oPos pos <> text "\n"
rHaskell (Quote xs) = parens $ oConcat $ map rMarxup xs
rHaskell (List xs) = brackets $ rHaskells xs
rHaskell (Parens xs) = parens $ rHaskells xs
rHaskell (String xs) = doubleQuotes $ text xs

rMarxup :: MarXup -> Doc
rMarxup (TextChunk s) = oText s
rMarxup (Unquote var val) = maybe mempty (\x -> text (x <> "<-")) var <>
                            text "element" <+> parens (hcat $ map (parens . rHaskells) val)
rMarxup (Comment _) = mempty
rMarxup (TextLn pos) = oText "\n" <> oPos pos

main :: IO ()
main = do
  x : y : z : _ <- getArgs
  putStrLn x
  putStrLn y
  putStrLn z
  parseFile y $ \res -> writeFile z $ render (rHaskells res)

