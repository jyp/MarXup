{-# LANGUAGE TupleSections, FlexibleInstances, TransformListComp #-}
import Text.ParserCombinators.Parsek.Position

import Data.Char
import Data.List
import System.IO
import System.Environment
import Control.Monad
import Data.DList hiding (map,foldr)
import Data.Monoid
import GHC.Exts (the,groupWith)
import Config

-- todo: parse haskell comments

------------------
-- Simple printing combinators, which do not add nor remove line breaks

type Doc = DList Char

text s = DL (s ++)
x <+> y =  x <> text " " <> y
oChar c = DL (c:)
parens s = oChar '(' <> s <> oChar ')'
braces s = oChar '{' <> s <> oChar '}'
brackets s = oChar '[' <> s <> oChar ']'
doubleQuotes s = oChar '"' <> s <> oChar '"'

int x = text $ show x
hcat :: [Doc] -> Doc
hcat = foldr (<>) mempty
punctuate t = map (<> t)
render :: Doc -> String
render x = unDL x ""

------------------------------------------
-- Output combinators

oPos :: SourcePos -> Doc
oPos EOF = mempty
oPos p = text "{-# LINE" <+> int (sourceLine p) <+> text (show (sourceName p)) <+> text "#-}"

oText :: String -> Doc
oText x = text "textual" <+> text (show x)

oMappend :: [Doc] -> Doc
oMappend [] = text "(return ())"
oMappend [x] = parens x
oMappend l = text "do" <+> braces (text "rec" <+> braces (hcat (punctuate (text ";") binds)) <> text ";" <> ret)
  where binds = init l
        ret = last l

----------------------------------------------
-- Parsing helpers

satisfy' :: (String -> Bool) -> Parser Char
satisfy' p = do
  l <- look
  unless (p l) $
    fail "Unexpected leading string"
  anySymbol

munch',munch1' :: (String -> Bool) -> Parser String
munch' p = scan =<< look
 where
  scan (c:cs) | p (c:cs) = (:) <$> anySymbol <*> scan cs
  scan _            = pure []

munch1' p = (:) <$> satisfy' p <*> munch' p

-- | A chunck not containing some strings
pChunk' :: [String] -> Parser String
pChunk' stops = munch1' (\l -> not $ any (`isPrefixOf` l) stops)


-- | A chunk not containing some chars.
pChunk :: [Char] -> Parser String
pChunk stops = munch1 (not . (`elem` stops))

----------------------------------------------
-- Parsing combinators

anyQuoteStrings :: [String]
anyQuoteStrings = concatMap (\(x,y) -> [x,y]) quoteStrings

pTextChunk = oText <$> pChunk' ("\n" : commentString : antiQuoteStrings ++ anyQuoteStrings) <?> "Text chunk"
pHaskChunk = text <$> pChunk' (map box "\n\"[]()" ++ map fst quoteStrings) <?> "Haskell chunk"
    -- we keep track of balancing

pWPos :: Parser Doc
pWPos = do
  char '\n'
  pos <- getPosition
  return $ oPos pos <> text "\n"

pHaskLn = pWPos -- before each newline, tell GHC where we are.
pTextLn = (oText "\n" <>) <$> pWPos
  -- add code to output a newline; and insert a newline in the code.

box = (:[])

pString :: Parser Doc
pString = do
  char '"'
  result <- many (string "\\\"" <|> pChunk ['"'])
  char '"'
  return $ doubleQuotes . hcat . map text $ result

-- | Parse some Haskell code with markup inside.
pHask :: Parser Doc
pHask =
  (hcat <$> many ((brackets <$> pArg "[]") <|>
                  (parens   <$> pArg "()") <|>
                  (parens   <$> pTextArg ) <|> pString <|> pHaskChunk <|> pHaskLn))

-- | Parse a text argument to an element
pTextArg' :: String -> String -> Parser Doc
pTextArg' open close = label "quoted text" $
  string open *>
  (oMappend <$> many (pElement <|> pTextChunk <|> pTextLn <|> pComment))
  <* string close

pTextArg :: Parser Doc
pTextArg = choice $ map (uncurry pTextArg') quoteStrings

pArg :: String -> Parser Doc
pArg [open,close] = char open *> pHask <*  char close

isIdentChar :: Char -> Bool
isIdentChar x = isAlphaNum x || (x `elem` "\'_")

pIdent :: Parser Doc
pIdent = text <$> munch1 isIdentChar <?> "identifier"

pArgument :: Parser Doc
pArgument = (pArg "()" <|> (brackets <$> pArg "[]") <|> pTextArg <|> pString) <?> "argument"

pElement :: Parser Doc
pElement = label "Haskell element" $ do
  choice $ map string $ antiQuoteStrings
  var <- ((<+> text "<-") <$> (pIdent <* string "<-")) <<|> pure mempty
  val <- ((:) <$> pIdent <*> manyGreedy pArgument) <|> (box <$> pArg "()")
  return $ var <> text "element" <+> parens (hcat $ fmap parens val)

commentString :: String
commentString = "%%"

pComment :: Parser Doc
pComment = label "Comment" $ do
  string commentString
  munch (/= '\n')
  return mempty

main :: IO ()
main = do
  x : y : z : _ <- getArgs
  putStrLn x
  putStrLn y
  putStrLn z
  p <- parseFromFile (pHask <* endOfFile) completeResults y
  case p of
    Left e -> handleErr e
    Right [res] -> writeFile z $ render res
    Right _ -> hPutStrLn stderr "Amibiguous input!"

handleErr e =
   sequence_
          [ hPutStrLn stderr (show $ maybePosToPos $ the pos) >>
            hPutStrLn stderr ("  Expected:" ++ (intercalate " or " $ nub what))
           | (exps,_why) <- e, (what,pos) <- exps, then group by pos using groupWith, then reverse ]

instance Show (DList Char) where
  show x = show $ render x


-- Tests
testHask = parse "<interactive>" pHask completeResults "arst « text @z<-fct[x](y) awft"
testHask2 = parse "<interactive>" pHask completeResults "ars(t) « text @z<-fct[x](y) » awft"
testText2 = parse "<interactive>" pTextArg completeResults "« text @fct(x »"
testText3 = parse "<interactive>" pTextArg completeResults "« 1 @x 2 @y 3 @x 4 »"
testElem = parse "<interactive>" pElement completeResults "@x<-fct(x « yop »)[y]"
testChunk = parse "<interactive>" pHaskChunk completeResults "t"
testArg = parse "<interactive>" (pArg "()") completeResults "()"

terr e  = case e of
    Left x -> handleErr x
    Right [res] -> putStrLn $ render res
