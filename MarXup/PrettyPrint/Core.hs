{-# LANGUAGE RecordWildCards #-}
module MarXup.PrettyPrint.Core(Doc(..),pretty,group,flatten,space,spacing,BoxTex(..)) where

import Data.Monoid
import MarXup (textual)
import MarXup.Latex
import MarXup.Tex
import MarXup.MultiRef
import MarXup.Diagram.Layout (BoxSpec(..))
import MarXup.Diagram.Tikz (showDistance)
import Data.Foldable (forM_)
import Control.Applicative
import Data.Function (on)
import Data.List (partition,minimumBy,sort)
import Data.Either (partitionEithers)


data BoxTex = TeX TeX BoxSpec | Spacing Double


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

toksToSimple [] = SEmpty
toksToSimple (Ln d:ts) = SLine d $ toksToSimple ts
toksToSimple (Tx s:ts) = SText s $ toksToSimple ts

instance Monoid Doc where
  mempty = Empty
  mappend = Cat
  
group :: Doc -> Doc
group x         = Union (flatten x) x

flatten :: Doc -> Doc
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Line noSpace)  = if noSpace then Empty else Text (Spacing 3.5)
flatten (Union x _y)    = flatten x
flatten (Column f)      = Column (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten other           = other                     --Empty,Char,Text



space = spacing 3.5

spacing = Text . Spacing
-- d = Text (hspace (showDistance d),BoxSpec {boxWidth = d, boxHeight = 0, boxDepth = 0})


len (TeX _ b)= boxWidth b
len (Spacing x) = x
hei (TeX _ x) = boxHeight x + boxDepth x
hei (Spacing _) = 0

-- | Does the first line of the document fit in the given width?
fits w _x        | w < 0         = False
fits _w SEmpty                   = True
fits w (SText s x)            = fits (w - len s) x
fits w (SLine i x)              = True


data Docs   = Nil
            | Cons !Double Doc Docs



data Token = Ln Double | Tx BoxTex

data Process = Process {overflow :: Double
                       ,curIndent :: !Double -- current indentation
                       ,numToks :: Int -- total number of non-space tokens produced
                       ,tokens :: [Token] -- tokens produced, in reverse order
                       ,rest :: !Docs -- rest of the input document to process
                       }
-- The ⟨filtering⟩ step takes all process states starting at the current
-- line, and discards those that are dominated.  A process state
-- dominates another one if it was able to produce more tokens while
-- having less indentation. This is implemented by first sorting the
-- process states by following 'measure', and then applying the
-- 'filtering' function.

measure =  \Process{..} -> (curIndent, negate numToks)
instance Eq Process where
  (==) = (==) `on` measure
instance Ord Process where
  compare = compare `on` measure

filtering :: [Process] -> [Process]
filtering (x:y:xs) | numToks x >= numToks y = filtering (x:xs)
                   | otherwise = x:filtering (y:xs)
filtering xs = xs

renderAll :: Double -> Double -> Doc -> SimpleDoc
renderAll rfrac w doc = toksToSimple $ reverse $ loop [Process 0 0 0 [] $ Cons 0 doc Nil]
    where
      loop ps = case dones of
        ((_,done):_) -> done -- here we could attempt to do a better choice. Seems to be fine for now.
        [] -> case conts of
          (_:_) -> loop $ filtering $ sort $ conts -- See the comment ⟨filtering⟩ above
          [] -> case conts'over of
            (_:_) -> loop [minimumBy (compare `on` overflow) conts'over]
            [] -> case dones'over of
              ((_,done):_) -> done
              [] -> [Tx $ TeX (textual "Pretty print: Panic") (BoxSpec 0 0 0)]

        where
          -- advance all processes by one line (if possible)
          ps' = concatMap (\Process{..} -> rall numToks tokens curIndent curIndent rest) ps
          -- Have some processes reached the end?
          (dones0,conts0) = partitionEithers ps'
          (conts,conts'over) = partition (\p -> overflow p <= 0) conts0
          (dones,dones'over) = partition (\(o,_) -> o <= 0) dones0

      -- r :: the ribbon width in characters
      r  = max 0 (min w (w * rfrac))

      -- Automatically inserted spacing does not count as doing more production.
      count (Spacing _) = 0
      count (TeX _ _) = 1

      -- Compute the state(s) after reaching the end of line.
      -- rall :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      rall :: Int -> [Token] -> Double -> Double -> Docs -> [Either (Double,[Token]) Process]
      rall nts ts n k ds0 = case ds0 of
         Nil ->  [Left $ (overflow,ts)] -- Done!
         Cons i d ds -> case d of
            Empty       -> rall nts ts n k ds
            Text s      -> let k' = k+len s in seq k' (rall (nts+count s) (Tx s:ts) n k' ds)
            Line _      -> [Right $ Process overflow i nts (Ln i:ts) ds] -- "yield" when the end of line is reached
            Cat x y     -> rall nts ts n k (Cons i x (Cons i y ds))
            Nest j x    -> let i' = i+j in seq i' (rall nts ts n k (Cons i' x ds))
            Union x y   -> rall nts ts n k (Cons i x ds) ++ rall nts ts n k (Cons i y ds)
            Column f    -> rall nts ts n k (Cons i (f k) ds)
            Nesting f   -> rall nts ts n k (Cons i (f i) ds)
        where overflow = negate $ min (w - k) (r - k + n)

-- countLines :: SimpleDoc -> Int
-- countLines = length . linify0

-- measureWidth :: SimpleDoc -> Double
-- measureWidth = maximum . map lineWidth . linify0
--   where lineWidth (i,boxes) = i + sum (map len boxes)

-- The original function implemented by Daan Leijen. Wadler derives a
-- similar function in his paper ("A prettier printer"). Unfortunately
-- it does not produce pretty layouts: the assumptions that enable the
-- amount of greediness implemented here do not hold after adding the
-- Column and Nesting combinators. So this function tends to generate
-- layouts with a long first line and a cramped column of stuff.

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

linify0 :: SimpleDoc -> [(Double, [BoxTex])]
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
  let strut = TeX mempty strutBox
  forM_ (heights strut $ linify0 d) $ \(y,(i,boxes)) -> do
    let mh = maximum (map hei (strut:boxes))
    forM_ (zip boxes $ computeWidths i boxes) $ \(tx,x) -> do
      case tx of
        Spacing _ -> return ()
        TeX t box -> do
          let y' = y - (mh - boxHeight box)
          tex $ "\\node[anchor=north west,inner sep=0] at (" ++ showDistance x ++ "," ++ showDistance y' ++ ")"
          braces $ t
          tex ";\n"

pretty :: Double -> Double -> Doc -> TeX
pretty rfrac w d = do
  layout $ renderAll rfrac w d


-- getBoxes :: Doc -> Tex Doc
-- getBoxes Empty = pure Empty
-- getBoxes (Cat d e) =  Cat <$> getBoxes d <*> getBoxes e
-- getBoxes (Union d e) =  Union <$> getBoxes d <*> getBoxes e
-- getBoxes (Tex (t,_)) = do
--   b <- justBox t
--   return (TEXT (t,b))
-- getBoxes (NEST i d) = NEST i <$> getBoxes d
-- getBoxes LINE = pure LINE
