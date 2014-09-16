{-# LANGUAGE RecordWildCards #-}
module MarXup.PrettyPrint.Core(Doc(..),pretty,group,flatten,space,spacing) where

import Data.Monoid
import MarXup (textual)
import MarXup.Latex
import MarXup.Tex
import MarXup.MultiRef 
import MarXup.Diagram.Tikz (showDistance)
import Data.Foldable (forM_)
import Control.Applicative
import Data.Function (on)
import Data.List (minimumBy)
type BoxTex = (TeX,BoxSpec)


data Doc        = Empty
                | Text BoxTex
                | Line !Bool            -- True <=> when undone by group, do not insert a space
                | Cat Doc Doc
                | Nest Double Doc
                | Union Doc Doc           -- invariant: first lines of first doc longer than the first lines of the second doc
                | Column  (Double -> Doc)
                | Nesting (Double -> Doc)


-- | Document "process"
data SimpleDoc  = SEmpty
                | SText BoxTex SimpleDoc
                | SLine Double SimpleDoc -- Line, indented.

instance Monoid Doc where
  mempty = Empty
  mappend = Cat
  
group :: Doc -> Doc
group x         = Union (flatten x) x

flatten :: Doc -> Doc
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Line noSpace)  = if noSpace then Empty else space
flatten (Union x _y)    = flatten x
flatten (Column f)      = Column (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten other           = other                     --Empty,Char,Text



space = spacing 3.5

spacing d = Text (hspace (showDistance d),BoxSpec {boxWidth = d, boxHeight = 0, boxDepth = 0})


len = boxWidth . snd
hei (_,x) = boxHeight x + boxDepth x

-- | Does the first line of the document fit in the given width?
fits w _x        | w < 0         = False
fits _w SEmpty                   = True
fits w (SText s x)            = fits (w - len s) x
fits w (SLine i x)              = True


data Docs   = Nil
            | Cons !Double Doc Docs

type Measure = Int
merge :: [(Measure,SimpleDoc)] -> [(Measure,SimpleDoc)] -> [(Measure,SimpleDoc)]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) = case (compare `on` fst) x y of
  LT -> x:merge xs (y:ys)
  GT -> y:merge (x:xs) ys
  EQ -> x:y:merge xs ys

(#) :: [a] -> (a -> b) -> [b]
(#) = flip fmap

data Token = Ln Double | Tx BoxTex
data Process = Process {tokens :: [Token] -- in reverse order
                       ,curCol :: !Double
                       ,curLine :: !Int
                       ,rest :: !Docs
                       }

processOne :: Process -> Either [Token] [Process]
processOne (Process{..}) = case rest of
  Nil -> Left $ reverse $ tokens
  Cons i d ds -> case d of
    Empty -> one $ Process{rest=ds,..}
    Text s -> one $ Process{curCol = curCol + len s, tokens = Tx s:tokens,..}
    Line _ -> Right $ [Process{curCol = i, curLine = curLine+1, tokens = Ln i:tokens,..}]
    Cat x y -> one $ Process{rest = Cons i x $ Cons i y $ ds,..}
    Nest j x -> one $ Process{rest = Cons (i+j) x ds,..}
    Union x y -> Right $ [Process{rest = Cons i x ds,..}
                 ,Process{rest = Cons i y ds,..}]
    Column f -> one $ Process {rest = Cons i (f curCol) ds,..}
    Nesting f -> one $ Process {rest = Cons i (f i) ds,..}
  where one = processOne

renderAll :: Double -> Double -> Doc -> [(Int,SimpleDoc)]
renderAll rfrac w doc = rall 0 0 (Cons 0 doc Nil)
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (w * rfrac))

      -- rall :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      rall :: Double -> Double -> Docs -> [(Measure,SimpleDoc)]
      rall n k _ |  min (w - k) (r - k + n) < 0 = []
      rall _n _k Nil      = [(0,SEmpty)]
      rall n k (Cons i d ds)
        = case d of
            Empty       -> rall n k ds
            Text s    -> let k' = k+len s in seq k' (rall n k' ds # \(l,x) -> (l,SText s x))
            Line _      -> rall i i ds # \(l,x) -> (l+1,SLine i x)
            Cat x y     -> rall n k (Cons i x (Cons i y ds))
            Nest j x    -> let i' = i+j in seq i' (rall n k (Cons i' x ds))
            Union x y   -> rall n k (Cons i x ds) `merge` rall n k (Cons i y ds)
            Column f    -> rall n k (Cons i (f k) ds)
            Nesting f   -> rall n k (Cons i (f i) ds)

      --nicest :: r = ribbon width, w = page width,
      --          n = indentation of current line, k = current column
      --          x and y, the (simple) documents to chose from.
      --          precondition: first lines of x are longer than the first lines of y.

countLines :: SimpleDoc -> Int
countLines = length . linify0


measureWidth :: SimpleDoc -> Double
measureWidth = maximum . map lineWidth . linify0
  where lineWidth (i,boxes) = i + sum (map (boxWidth . snd) boxes)

renderPrettiest rfrac w doc = case allLayouts of
   [] -> SText (textual "OVERFLOW", BoxSpec 0 0 0) SEmpty -- ALT: fallback to renderPretty
   (_,best):_ -> best
 where allLayouts = renderAll rfrac w doc
         
       

renderPretty :: Double -> Double -> Doc -> SimpleDoc
renderPretty rfrac w doc
    = best 0 0 (Cons 0 doc Nil)
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (w * rfrac))

      -- best :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best _n _k Nil      = SEmpty
      best n k (Cons i d ds)
        = case d of
            Empty       -> best n k ds
            Text s    -> let k' = k+len s in seq k' (SText s (best n k' ds))
            Line _      -> SLine i (best i i ds)
            Cat x y     -> best n k (Cons i x (Cons i y ds))
            Nest j x    -> let i' = i+j in seq i' (best n k (Cons i' x ds))
            Union x y   -> nicest n k (best n k (Cons i x ds))
                                      (best n k (Cons i y ds))

            Column f    -> best n k (Cons i (f k) ds)
            Nesting f   -> best n k (Cons i (f i) ds)

      --nicest :: r = ribbon width, w = page width,
      --          n = indentation of current line, k = current column
      --          x and y, the (simple) documents to chose from.
      --          precondition: first lines of x are longer than the first lines of y.
      nicest n k x y    | fits width x  = x
                        | otherwise     = y
                        where
                          width = min (w - k) (r - k + n)

linify0 d = linify d 0 []

-- | Turn a simpledoc into a list of lines and indentation.
linify :: SimpleDoc -> Double -> [BoxTex] -> [(Double,[BoxTex])]
linify SEmpty i t = [(i,t)]
linify (SText s x) i t = linify x i (t ++ [s])
linify (SLine i' x) i t = (i,t):linify x i' mempty

computeHeights :: BoxTex -> [(Double,[BoxTex])] -> [Double]
computeHeights strut ls = scanl (\y (_i,boxes) -> y - maximum (map hei (strut:boxes))) 0 ls

heights :: BoxTex -> [(Double,[BoxTex])] -> [(Double,(Double, [BoxTex]))]
heights strut ls = zip (computeHeights strut ls) ls

computeWidths :: Double -> [BoxTex] -> [Double]
computeWidths = scanl (\x b -> len b + x)

layout :: SimpleDoc -> Tex ()
layout d = env "tikzpicture" $ do
  strutBox <- justBox $ -- textual "qM" -- slightly too small ones
                         cmd0 "strut" -- this gives slightly too big spaces sometimes
  let strut = (mempty,strutBox)
  forM_ (heights strut $ linify0 d) $ \(y,(i,boxes)) -> do
    let mh = maximum (map (boxHeight . snd) (strut:boxes))
    forM_ (zip boxes $ computeWidths i boxes) $ \((t,box),x) -> do
        let y' = y - (mh - boxHeight box)
        tex $ "\\node[anchor=north west,inner sep=0] at (" ++ showDistance x ++ "," ++ showDistance y' ++ ")"
        braces $ t
        tex ";\n"

pretty :: Double -> Double -> Doc -> TeX
pretty rfrac w d = do
  layout $ renderPrettiest rfrac w d


-- getBoxes :: Doc -> Tex Doc
-- getBoxes Empty = pure Empty
-- getBoxes (Cat d e) =  Cat <$> getBoxes d <*> getBoxes e
-- getBoxes (Union d e) =  Union <$> getBoxes d <*> getBoxes e
-- getBoxes (Tex (t,_)) = do
--   b <- justBox t
--   return (TEXT (t,b))
-- getBoxes (NEST i d) = NEST i <$> getBoxes d
-- getBoxes LINE = pure LINE
