import Text.ParserCombinators.Parsek.Position

import System.Environment
import Data.Monoid
import Data.DList hiding (foldr, map)
import MarXupParser
import qualified Literate as Lit
import Output
import Config

rHaskells :: [Haskell] -> Doc
rHaskells xs = mconcat $ map rHaskell xs

rHaskell :: Haskell -> DList Char
rHaskell (HaskChunk s) = text s
rHaskell (HaskLn pos) = oPos pos
rHaskell (Quote xs) = parens $ oConcat $ map rMarxup xs
rHaskell (List xs) = brackets $ rHaskells xs
rHaskell (Parens xs) = parens $ rHaskells xs
rHaskell (String xs) = doubleQuotes $ text xs

rArg :: (SourcePos, Haskell) -> Doc
rArg (pos,h) = oPos pos <> parens (rHaskell h)

rMarxup :: MarXup -> Doc
rMarxup QuotedAntiQuote = case antiQuoteStrings of
  x:xs -> oText x
rMarxup (TextChunk s) = oText s
rMarxup (Unquote var val) =
  maybe mempty (\(pos,x) -> oPos pos <> text (x <> "<-")) var <>
  text "element" <+> parens (hcat $ map rArg val)
rMarxup (Comment _) = mempty

main :: IO ()
main = do
  x : y : z : _ <- getArgs
  parseFile y $ \res -> writeFile z $ render (rHaskells res <> Lit.rHaskells res)

