{-# LANGUAGE TupleSections, FlexibleInstances, TransformListComp #-}
import Text.ParserCombinators.Parsek.Position
import Text.ParserCombinators.Class

import Data.Char
import Control.Applicative
import Data.List
import System.IO
import System.Environment
import Control.Monad
import Data.DList hiding (map,foldr)
import Data.Monoid
import GHC.Exts (the,groupWith)

-- todo: parse strings inside haskell code
-- todo: output line directives.
-- todo: haskell comments

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
oPos p = text "{-# LINE" <+> int (sourceLine p) <+> text (show (sourceName p)) <+> text "#-}"
    
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

-- | A chunk not containing some chars.
pChunk :: [Char] -> Parser String
pChunk stops = munch1 (not . (`elem` stops)) 

pTextChunk = oText <$> pChunk "@\n«»" <?> "Text chunk"
pHaskChunk = text <$> pChunk "\n\"«»[]()" <?> "Haskell chunk"
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
pTextArg :: Parser Doc
pTextArg = label "quoted text" $
  char '«' *>
  (oMappend <$> many (pElement <|> pTextChunk <|> pTextLn <|> pEscape))
  <* char '»'

pArg :: String -> Parser Doc
pArg [open,close] = char open *> pHask <*  char close

isIdentChar :: Char -> Bool
isIdentChar x = isAlphaNum x || (x `elem` "\'_")

pIdent :: Parser Doc
pIdent = text <$> munch1 isIdentChar <?> "identifier"

pArgument = (pArg "()" <|> (brackets <$> pArg "[]") <|> pTextArg) <?> "argument"

pEscape = do
  string "@@"
  return $ oText "@"

pElement :: Parser Doc
pElement = label "Haskell element" $ do
  char '@'
  var <- ((<+> text "<-") <$> (pIdent <* string "<-")) <<|> pure mempty
  val <- ((:) <$> pIdent <*> manyGreedy pArgument) <|> (box <$> pArg "()")
  return $ var <> text "element" <+> parens (hcat $ fmap parens val)

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

