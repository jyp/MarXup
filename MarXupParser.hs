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

data Haskell = HaskChunk String | HaskLn SourcePos | Quote [MarXup] | List [Haskell] | Parens [Haskell] | String String
data MarXup = TextChunk String | Unquote (Maybe String) [[Haskell]] | Comment String | TextLn SourcePos

----------------------------------------------
-- Parsing combinators

anyQuoteStrings :: [String]
anyQuoteStrings = concatMap (\(x,y) -> [x,y]) quoteStrings

pTextChunk = TextChunk <$> pChunk' ("\n" : commentString : antiQuoteStrings ++ anyQuoteStrings) <?> "Text chunk"
pHaskChunk = HaskChunk <$> pChunk' (map box "\n\"[]()" ++ map fst quoteStrings) <?> "Haskell chunk"
    -- we keep track of balancing

pWPos :: Parser SourcePos
pWPos = do
  char '\n'
  getPosition

pHaskLn = HaskLn <$> pWPos -- before each newline, tell GHC where we are.
pTextLn = TextLn <$> pWPos
  -- add code to output a newline; and insert a newline in the code.

box = (:[])

pString :: Parser Haskell
pString = do
  char '"'
  result <- many (string "\\\"" <|> pChunk ['"'])
  char '"'
  return $ String $ concat result

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
  (many (pElement <|> pTextChunk <|> pTextLn <|> pComment))
  <* string close)

pTextArg :: Parser Haskell
pTextArg = choice $ map (uncurry pTextArg') quoteStrings

pArg :: String -> Parser [Haskell]
pArg [open,close] = char open *> pHask <*  char close

isIdentChar :: Char -> Bool
isIdentChar x = isAlphaNum x || (x `elem` "\'_")

pIdent :: Parser String
pIdent = munch1 isIdentChar <?> "identifier"

pArgument :: Parser [Haskell]
pArgument = (pArg "()" <|> (pArg "[]") <|> (box <$> pTextArg) <|> (box <$> pString)) <?> "argument"

pId :: Parser [Haskell]
pId = (box . HaskChunk) <$> pIdent

pElement :: Parser MarXup
pElement = 
  label "Haskell element" $ do
    choice $ map string $ antiQuoteStrings
    var <- (Just <$> (pIdent <* string "<-")) <<|> pure Nothing
    val <- ((:) <$> pId <*> manyGreedy pArgument) <|> (box <$> pArg "()")
    return $ Unquote var val

commentString :: String
commentString = "%%"

pComment :: Parser MarXup
pComment = Comment <$> do
  label "Comment" $ do
    string commentString
    munch (/= '\n')
    string "\n"
    return mempty

parseFile :: String -> ([Haskell] -> IO ()) -> IO ()
parseFile fname k = do
  p <- parseFromFile (pHask <* endOfFile) completeResults fname
  case p of
    Left e -> handleErr e
    Right [res] -> k res
    Right _ -> hPutStrLn stderr "Amibiguous input!"

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


{-
-- Tests
testHask = parse "<interactive>" pHask completeResults "arst « text @z<-fct[x](y) awft"
testHask2 = parse "<interactive>" pHask completeResults "ars(t) « text @z<-fct[x](y) » awft"
testText2 = parse "<interactive>" pTextArg completeResults "« text @fct(x »"
testText3 = parse "<interactive>" pTextArg completeResults "« 1 @x 2 @y 3 @x 4 »"
testElem = parse "<interactive>" pElement completeResults "@x<-fct(x « yop »)[y]"
testChunk = parse "<interactive>" pHaskChunk completeResults "t"
testArg = parse "<interactive>" (pArg "()") completeResults "()"
-}
