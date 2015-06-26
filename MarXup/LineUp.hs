module MarXup.LineUp where

import Data.Function
import Data.List
import Data.Foldable
import Control.Monad (when)
import Language.Haskell.Exts.Lexer
import Language.Haskell.Exts.Parser (ParseResult(..))
import Language.Haskell.Exts.SrcLoc

import MarXup
import MarXup.Tex
import MarXup.Verbatim

haskell :: Verbatim a -> Tex ()
haskell v = case lexTokenStream (fromVerbatim v) of
  ParseOk toks -> lineup toks
  ParseFailed location err -> textual (show location ++ show err)

lineup :: [Loc Token] -> TeX
lineup input = env "ptboxed" $ do
  declColumn "B"
  forM_ (zip allTabStops [(1::Int)..]) $ \(_col,tab) -> 
    declColumn (show tab)
  declColumn "E"
  mapM_ printLine array
  where
    declColumn :: String -> TeX
    declColumn c = cmdn_ "column" [tex c,tex "@{}l@{}"]
    printLine :: [[Token]] -> TeX
    printLine xs = do
      forM_ (zip xs [(1::Int)..]) $ \ (ts,tab) -> when (not (null ts)) $ do
        cmdn' ">" [show tab] []
        forM_ ts printTok
      cmdn' "<" ["E"] []
      texLn "\\\\"
      return ()
    
    printTok :: Token -> TeX
    printTok t = cmd "ensuremath" $ cmd "mathnormal" $ tex $ showToken t
    
    startCol = srcSpanStartColumn . loc
    endCol = srcSpanEndColumn . loc

    array :: [[[Token]]]
    array = map tabify lins

    lins :: [[Loc Token]]
    lins = groupBy ((==) `on` (srcSpanStartLine . loc)) input

    isAligning :: [Loc Token] -> [(Bool,Loc Token)]
    isAligning [] = []
    isAligning (x:xs) = (True,x) :
                        [(startCol t2 > 1 + endCol t1,t2) | (t1,t2) <- zip (x:xs) xs]

    tabStops :: [Loc Token] -> [Int]
    tabStops xs = [startCol x | (align,x) <- isAligning xs, align]

    allTabStops :: [Int]
    allTabStops = sort $ nub $ concatMap tabStops lins

    tabify :: [Loc Token] -> [[Token]]
    tabify xs = tabify' (isAligning xs) allTabStops

    clearMeta :: [(Bool,Loc Token)] -> [Token]
    clearMeta = map (unLoc . snd)

    tabify' :: [(Bool,Loc Token)] -> [Int]-> [[Token]]
    tabify' [] _ = []
    tabify' xs [] = [clearMeta xs]
    tabify' xs (t:ts) = clearMeta col:tabify' xs' ts
      where (col,xs') = break (\(align,s) -> align && (startCol s >= t)) xs

