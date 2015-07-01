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

int x = text $ show x
render :: Doc -> String
render = toList

------------------------------------------
-- Output combinators

oPos :: SourcePos -> Doc
oPos EOF = mempty
oPos p = text "{-# LINE" <+> int (sourceLine p) <+> text (show (sourceName p)) <+> text "#-}"

----------------------------------------------
-- Top-level generation

rHaskells :: [Haskell] -> Doc
rHaskells xs = mconcat $ map rHaskell xs

rHaskell :: Haskell -> DList Char
rHaskell (HaskLn pos) = oPos pos <> text "\n"
rHaskell (Quote xs) = mconcat $ map rMarxup xs
rHaskell _ = mempty

rMarxup :: MarXup -> Doc
rMarxup (TextLn pos) = oPos pos <> text "\n"
rMarxup (Unquote _ [[HaskChunk "haskell"],[Quote code]]) = mconcat $ map rInlineHask code
rMarxup _ = mempty

rInlineHask :: MarXup -> Doc
rInlineHask (TextLn pos) = oPos pos <> text "\n"
rInlineHask (TextChunk x) = text x
rInlineHask _ = mempty

main :: IO ()
main = do
  x : y : z : _ <- getArgs
  putStrLn x
  putStrLn y
  putStrLn z
  parseFile y $ \res -> writeFile z $ render (rHaskells res)

