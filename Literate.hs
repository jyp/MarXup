module Literate where

import Data.Monoid
import Data.DList hiding (foldr, map)
import MarXupParser
import Data.List (isPrefixOf)
import Output
import Config

----------------------------------------------
-- Top-level generation

rHaskells :: [Haskell] -> Doc
rHaskells xs = mconcat $ map rHaskell xs

rHaskell :: Haskell -> DList Char
rHaskell (HaskLn pos) = oPos pos <> text "\n"
rHaskell (Quote xs) = mconcat $ map rMarxup xs
rHaskell _ = mempty

rMarxup :: MarXup -> Doc
rMarxup (Unquote _ [(_,HaskChunk fct),(position,Quote code)]) | "haskell" `isPrefixOf` fct = oPos position <> foldMap rInlineHask code
rMarxup _ = mempty

rInlineHask :: MarXup -> Doc
rInlineHask (TextChunk x) = text x
rInlineHask QuotedAntiQuote = case antiQuoteStrings of
  x:_ -> text x
rInlineHask _ = mempty

