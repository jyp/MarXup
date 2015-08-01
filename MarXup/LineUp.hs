module MarXup.LineUp where

import Data.List
import Data.Foldable
import Control.Monad (when)

import MarXup.Tex

data Tok = Tok {
  startCol :: Int,
  endCol :: Int,
  render :: TeX
  }

lineup :: [[Tok]] -> TeX
lineup input = do
  cmd0 "noindent"
  cmd "ensuremath" $ env "pboxed" $ do
    declColumn "B"
    forM_ (zip allTabStops [(1::Int)..]) $ \(_col,tab) -> 
      declColumn (show tab)
    declColumn "E"
    texLn "%"
    mapM_ printLine array
  where
    showCol 0 = "B"
    showCol n = show n
    declColumn :: String -> TeX
    declColumn c = cmdn_ "column" [tex c,tex "@{}l@{\\;}"]

    lastIndex :: (a -> Bool) -> [a] -> Int
    lastIndex p xs = length (takeWhile p xs) - 1

    printLine :: [[Tok]] -> TeX
    printLine xs = do
      let lastEmpty = lastIndex null xs
      forM_ (zip xs [(0::Int)..]) $ \(ts,colName) -> do
         if (null ts)
             then when (colName == lastEmpty && colName > 0) $ do
                    cmdn' ">" [showCol colName] []
                    cmd0 "quad"
             else do cmdn' ">" [showCol colName] []
                     forM_ ts $ \t -> do 
                       render t
                       tex "\\;"
      cmdn' "<" ["E"] []
      texLn "\\\\"
      return ()

    -- The input, grouped in lines and columns
    array :: [[[Tok]]]
    array = map tabify input

    -- Is the token preceded by two spaces or starts a line?
    isAligning :: [Tok] -> [(Bool,Tok)]
    isAligning [] = []
    isAligning (x:xs) = (True,x) :
                        [(startCol t2 > 1 + endCol t1,t2) | (t1,t2) <- zip (x:xs) xs]

    -- The tab stops in a line
    tabStops :: [Tok] -> [Int]
    tabStops xs = [startCol x | (align,x) <- isAligning xs, align]

    -- all the tab stops
    allTabStops :: [Int]
    allTabStops = sort $ nub $ concatMap tabStops input

    tabify :: [Tok] -> [[Tok]]
    tabify xs = tabify' (isAligning xs) allTabStops

    clearMeta :: [(Bool,Tok)] -> [Tok]
    clearMeta = map snd

    tabify' :: [(Bool,Tok)] -> [Int]-> [[Tok]]
    tabify' [] _ = []
    tabify' xs [] = [clearMeta xs]
    tabify' xs (t:ts) = clearMeta col:tabify' xs' ts
      where (col,xs') = break (\(align,s) -> align && (startCol s >= t)) xs

