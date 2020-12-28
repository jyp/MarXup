{-# LANGUAGE TupleSections, FlexibleInstances, TransformListComp #-}

module MarXupParser (parseFile, Haskell(..), MarXup(..)) where

import Text.ParserCombinators.Parsek.Position
import Data.Char
import Data.List
import System.IO
import Control.Monad
import GHC.Exts (the,groupWith)
import Config

-- todo: parse haskell comments (so that marxup there is not recognized)

------------------
-- Simple printing combinators, which do not add nor remove line breaks

data Haskell = HaskChunk String | HaskLn SourcePos | Quote [MarXup] | List [Haskell] | Parens [Haskell] | String String deriving (Show)
data MarXup = QuotedAntiQuote | TextChunk String | Unquote (Maybe (SourcePos,Haskell)) [(SourcePos,Haskell)] | Comment String deriving (Show)

----------------------------------------------
-- Parsing combinators

anyQuoteStrings :: [String]
anyQuoteStrings = concatMap (\(x,y) -> [x,y]) quoteStrings

pTextChunk :: Parser MarXup
pTextChunk = TextChunk <$> pChunk' (commentString : antiQuoteStrings ++ anyQuoteStrings) <?> "Text chunk"

pHaskChunk :: Parser Haskell
pHaskChunk = HaskChunk <$> pChunk' (map box "\n\"[]()" ++ map fst quoteStrings) <?> "Haskell chunk"
    -- we keep track of balancing

pWPos :: Parser SourcePos
pWPos = do
  _ <- char '\n'
  getPosition

withPos :: Parser a -> Parser (SourcePos,a)
withPos p = do
  pos <- getPosition
  x <- p
  return (pos,x)

pHaskLn :: Parser Haskell
pHaskLn = HaskLn <$> pWPos -- before each newline, tell GHC where we are.

box :: a -> [a]
box = (:[])

pString :: Parser Haskell
pString = do
  _ <- char '"'
  result <- many (string "\\\"" <|> pChunk ['"'])
  _ <- char '"'
  return $ String $ concat result

pPattern :: Parser Haskell
pPattern = (List <$> pArg "[]") <|>
           (Parens <$> pArg "()") <|>
           pId

pArgument :: Parser Haskell
pArgument = (Parens <$> pArg "()" <|> (List <$> pArg "[]") <|> pTextArg <|> pString) <?> "argument"

-- | Parse some Haskell code with markup inside.
pHask :: Parser [Haskell]
pHask = many ((List <$> pArg "[]") <|>
          (Parens <$> pArg "()") <|>
          pTextArg  <|>
          pString <|>
          pHaskChunk <|>
          pHaskLn)

-- | Parse a text argument to an element
pTextArg' :: String -> String -> Parser Haskell
pTextArg' open close = Quote <$> (label "quoted text" $
  string open *>
  (many (pQuotedAntiQuote <|> pElement <|> pTextChunk <|> pComment))
  <* string close)

pTextArg :: Parser Haskell
pTextArg = choice $ map (uncurry pTextArg') quoteStrings

pArg :: String -> Parser [Haskell]
pArg [open,close] = char open *> pHask <*  char close

isIdentChar :: Char -> Bool
isIdentChar x = isAlphaNum x || (x `elem` "\'_")

pIdent :: Parser String
pIdent = munch1 isIdentChar <?> "identifier"


pId :: Parser Haskell
pId = HaskChunk <$> pIdent

pQuotedAntiQuote :: Parser MarXup
pQuotedAntiQuote = do
  _ <- choice $ map (string . double) antiQuoteStrings
  return QuotedAntiQuote
  where double x = x ++ x

pElement :: Parser MarXup
pElement = 
  label "Haskell element" $ do
    _ <- choice $ map string $ antiQuoteStrings
    var <- (Just <$> (withPos pPattern <* string "<-")) <<|> pure Nothing
    val <- ((:) <$> withPos pId <*> manyGreedy (withPos pArgument)) <|>
           (box <$> withPos (Parens <$> pArg "()"))
    return $ Unquote var val

commentString :: String
commentString = "%%"

pComment :: Parser MarXup
pComment = Comment <$> do
  label "Comment" $ do
    _ <- string commentString
    _ <- munch (/= '\n')
    _ <- string "\n"
    return mempty

parseFile :: String -> ([Haskell] -> IO ()) -> IO ()
parseFile fname k = do
  p <- parseFromFile (pHask <* endOfFile) completeResults fname
  case p of
    Left e -> handleErr e
    Right [res] -> k res
    Right _ -> hPutStrLn stderr "Amibiguous input!"

handleErr :: [([([Char], Maybe SourcePos)], b)] -> IO ()
handleErr e =
   sequence_
          [ hPutStrLn stderr (show $ maybePosToPos $ the pos) >>
            hPutStrLn stderr ("  Expected:" ++ (intercalate " or " $ nub what))
           | (exps,_why) <- e, (what,pos) <- exps, then group by pos using groupWith, then reverse ]

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

-- | A chunk not containing some strings
pChunk' :: [String] -> Parser String
pChunk' stops = munch1' (\l -> not $ any (`isPrefixOf` l) stops)


-- | A chunk not containing some chars.
pChunk :: [Char] -> Parser String
pChunk stops = munch1 (not . (`elem` stops))


-- Tests
testHask = parse "<interactive>" pHask completeResults "arst « text @z<-fct[x](y) awft"
testHask2 = parse "<interactive>" pHask completeResults "ars(t) « text @z<-fct[x](y) » awft"
testText2 = parse "<interactive>" pTextArg completeResults "« text @fct(x »"
testText3 = parse "<interactive>" pTextArg completeResults "« 1 @x 2 @y 3 @x 4 »"
testText1 = parse "<interactive>" pTextArg completeResults "« arst @(x,y)<-fct«something» qwfp  »"
testText4 = parse "<interactive>" pTextArg completeResults "« qwfp @([_,_,introExample3Fig],introExamplesAllFig)<-exsFigs  arst »"

testElem = parse "<interactive>" pElement completeResults "@x<-fct(x « yop »)[y]"
testChunk = parse "<interactive>" pHaskChunk completeResults "t"
testArg = parse "<interactive>" (pArg "()") completeResults "()"

-- >>> testHask
-- Left [([("\"@\"",Nothing),("\"@@\"",Nothing),("quoted text",Just <interactive>:1:5)],"satisfy"),([("quoted text",Just <interactive>:1:5)],"mzero"),([("\"@\"",Nothing),("\"@\"",Nothing),("Haskell element",Nothing),("quoted text",Just <interactive>:1:5)],"satisfy"),([("Haskell element",Nothing),("quoted text",Just <interactive>:1:5)],"mzero"),([("Text chunk",Nothing),("quoted text",Just <interactive>:1:5)],"satisfy"),([("\"%\"",Nothing),("\"%%\"",Nothing),("Comment",Nothing),("quoted text",Just <interactive>:1:5)],"satisfy"),([("\"\\187\"",Nothing),("\"\\187\"",Nothing),("quoted text",Just <interactive>:1:5)],"satisfy")]

-- >>> testHask2
-- Right [[HaskChunk "ars",Parens [HaskChunk "t"],HaskChunk " ",Quote [TextChunk " text ",Unquote (Just (<interactive>:1:15,HaskChunk "z")) [(<interactive>:1:18,HaskChunk "fct"),(<interactive>:1:21,List [HaskChunk "x"]),(<interactive>:1:24,Parens [HaskChunk "y"])],TextChunk " "],HaskChunk " awft"]]

-- >>> testText1
-- Right [Quote [TextChunk " arst ",Unquote (Just (<interactive>:1:8,Parens [HaskChunk "x,y"])) [(<interactive>:1:15,HaskChunk "fct"),(<interactive>:1:18,Quote [TextChunk "something"])],TextChunk " qwfp  "]]

-- >>> testText2
-- Right [Quote [TextChunk " text ",Unquote Nothing [(<interactive>:1:8,HaskChunk "fct")],TextChunk "(x "]]

-- >>> testText3
-- Right [Quote [TextChunk " 1 ",Unquote Nothing [(<interactive>:1:5,HaskChunk "x")],TextChunk " 2 ",Unquote Nothing [(<interactive>:1:10,HaskChunk "y")],TextChunk " 3 ",Unquote Nothing [(<interactive>:1:15,HaskChunk "x")],TextChunk " 4 "]]

-- >>> testText4
-- Right [Quote [TextChunk " qwfp ",Unquote (Just (<interactive>:1:8,Parens [List [HaskChunk "_,_,introExample3Fig"],HaskChunk ",introExamplesAllFig"])) [(<interactive>:1:54,HaskChunk "exsFigs")],TextChunk "  arst "]]

-- Local Variables:
-- dante-target: "marxup:exe:marxup"
-- End:
