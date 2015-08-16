{-# LANGUAGE RecordWildCards #-}
module MarXup.LineUp (Tok(..),lineup,mkSpaces) where

import Data.List
import Data.Foldable
import Control.Monad (when)
import Data.Monoid
import Data.Maybe (catMaybes)

import MarXup.Tex

data Tok = Tok {
  startCol :: Int,
  endCol :: Int,
  preSpace :: Float, -- max amount of space which should come before this token, in mu
  render :: TeX,
  postSpace :: Float -- max amount of space which should come after this token, in mu
  }


justIf True x = Just x
justIf _ _ = Nothing

marx True = '!'
marx False = '-'

lineup :: [[Tok]] -> TeX
lineup input = env'' "list" [] [mempty,tex "\\setlength\\leftmargin{1em}"] $ do
  texLn ""
  texLines $ map (("% " ++) . map marx . drop 1 . isIndentTab ) array
  
  texLn "\\item\\relax"
  cmd "ensuremath" $ env "pboxed" $ do
    declColumn Nothing "B"
    forM_ (zip3 allTabStops [(1::Int)..] (drop 1 indentColumns)) $ \(_col,tab,indenting) -> 
      declColumn (justIf (indenting) $ tex $ show (tab-1) ++ "em") (show tab)
    declColumn Nothing "E"
    texLn "%"
    sequence_ $ intersperse (texLn "\\\\") $ map printLine array
  where
    showCol 0 = "B"
    showCol n = show n
    declColumn :: Maybe TeX -> String -> TeX
    declColumn dim c = do
      cmdm "column" (catMaybes [dim]) [tex c,tex "@{}>{}l<{}@{}"]
      return ()

    printLine :: [[Tok]] -> TeX
    printLine xs = do
      forM_ (zip xs [(0::Int)..]) $ \(ts,colName) -> do
         when (not $ null ts) $ do
           cmdn' ">" [showCol colName] []
           braces $ forM_ ts $ \t -> do 
                render t
      cmdn' "<" ["E"] []
      return ()

    -- The input, grouped in lines and columns
    array :: [[[Tok]]]
    array = map (tabify . mkSpaces) input

    -- Is the token preceded by two spaces or starts a line?
    isAligning :: [Tok] -> [(Bool,Tok)]
    isAligning [] = []
    isAligning (x:xs) = (True,x) :
                        [(startCol t2 > 1 + endCol t1,t2) | (t1,t2) <- zip (x:xs) xs]

    -- | The tabstop possibly beginning an indentation? It cannot be
    -- if it both contains a token and is preceded by stuff.
    isIndentTab :: [[Tok]] -> [Bool]
    isIndentTab xs = zipWith (||) nulls (scanl (&&) True nulls) 
      where nulls = map null xs

    -- | Is a tabstop an indentation? (Take the intersection for all lines)
    indentColumns :: [Bool]
    indentColumns = map and $ transpose $ map isIndentTab array

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



--- | Transform a list of tokens to move the spacing info into the TeX
-- field of the tokens (spacing goes after the texts)
mkSpaces :: [Tok] -> [Tok]
mkSpaces [] = []
-- mkSpaces ts = [ Tok startCol endCol
--                 0 (tex (show preSpace) <> render <> tex (show postSpace) <> tex "\\;") 0
--               | Tok{..} <- ts]
mkSpaces ts = [ Tok (startCol l) (endCol l) 0
                (render l <>
                 tex ("\\mskip " ++ show (min (postSpace l) (preSpace r)) ++ "mu" )) 0
              | (l,r) <- zip ts (tail ts) ] ++ [last ts]
