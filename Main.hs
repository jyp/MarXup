{-# LANGUAGE TupleSections #-}
import Text.ParserCombinators.Parsec

import Data.Char
import Control.Applicative hiding (many, (<|>))
import Data.List
import System.IO
import System.Environment
import Control.Monad
import Data.DList hiding (map,foldr)
import Data.Monoid

-- todo: parse strings inside haskell code
-- todo: output line directives.

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
render x = unDL x ""

------------------------------------------
-- Output combinators
    
oPos :: SourcePos -> Doc
oPos p = text "{-# LINE" <+> int (sourceLine p) <+> text (show (sourceName p)) <+> text "#-}\n"
    
oText :: String -> Doc
oText x = text "textual" <+> text (show x)

oMappend :: [Doc] -> Doc
oMappend [] = text "(return ())"
oMappend [x] = parens x
oMappend l = text "do" <+> braces (text "rec" <+> braces (hcat (punctuate (text ";") binds)) <> text ";" <> ret)
  where binds = init l
        ret = last l
             

oBraces :: Doc -> Doc
oBraces x = oText "{" <> text "*>" <> x <> text "<*" <>  oText "}"


----------------------------------------------
-- Parsing combinators
pChunk :: String -> Parser String
pChunk stops = many1 (noneOf stops)

pWPos :: Parser Doc 
pWPos = do 
  char '\n'
  pos <- getPosition
  return $ oPos pos

pTextChunk = (oText <$> pChunk "\n{}@")

pHaskChunk = (text <$> many1 (noneOf "\n\"{}[]()@" <|> try (char '@' <* notFollowedBy (oneOf "[({\""))))

pHaskLn = pWPos -- before each newline, tell GHC where we are.

pTextLn = do
    (oText "\n" <>) <$> pWPos 
  -- add code to output a newline; and insert a newline in the code.
  

pQuote = do
  try (string "@\"")
  d <- pInText (string "@\"")
  return $ parens d

box x = [x]

pString = do
  char '"'
  result <- many (string "\\\"" <|> pChunk ['"'])
  char '"'
  return $ doubleQuotes . hcat . map text $ result

-- | Parse some Haskell code with markup inside. 
pHask = hcat <$> many ((brackets <$> pArg "[]") <|> 
                       (braces   <$> pArg "{}") <|>
                       (parens   <$> pArg "()") <|> pQuote <|> pString <|> pHaskChunk <|> pHaskLn)

-- | Parse a text argument to an element
pTextArg :: Parser Doc
pTextArg = do
  char '{' 
  result <- pInText (char '}')
  return result


pInText :: Parser a -> Parser Doc
pInText end = oMappend <$> manyTill (pTextChunk <|> pTextLn <|> pElement <|> (oBraces <$> pTextArg)) (try end)

pArg :: String -> Parser Doc
pArg [open,close] = do 
  char open
  result <- pHask
  char close
  return result

isIdentChar :: Char -> Bool
isIdentChar x = isAlphaNum x || (x `elem` "\'_")

-- | Either @fctName or @result<-fctName
pFctName :: Parser (Maybe Doc, Doc)
pFctName = do 
  x <- try (char '@' >> satisfy isIdentChar) -- this is to clash with @"
  xs' <- many1 (satisfy isIdentChar)
  let xs = x : xs'
  ys <- option Nothing (Just <$> (try (string "<-") >> many1 (satisfy isIdentChar)))
  return $ case ys of
    Nothing -> (Nothing, text xs)
    Just ys -> (Just (text xs), text ys)

pFct = (Nothing,) <$> parens <$> 
          (try (string "@{") *> pHask <* char '}')

pElement :: Parser Doc
pElement = do
  (result,function) <- pFctName <|> pFct
  args <- many (pArg "()" <|> (brackets <$> pArg "[]") <|> pTextArg)
  let binder = maybe mempty (<+> text "<-") result
  return $ binder <> text "element" <+> parens (function <+> (hcat $ fmap parens args))
{-
pTop :: Parser Doc
pTop = do
  t <- pInText
  return (text "import Text" $$
          text "main =" <+> t)
-}
{-
main = do
  p <- parseFromFile pTop "Text.ths"
  case p of 
    Left err -> hPutStrLn stderr $ show err
    Right res -> putStrLn $ render res
  -}

main :: IO ()
main = do
  x : y : z : _ <- getArgs
  putStrLn x
  putStrLn y
  putStrLn z
  p <- parseFromFile pHask y
  case p of 
    Left err -> do
           hPutStrLn stderr $ show err
    Right res -> writeFile z $ 
                    render res

