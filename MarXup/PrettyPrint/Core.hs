module MarXup.PrettyPrint.Core(DOC(..),pretty,group,flatten,space) where

import Control.Applicative
import Data.Monoid
import MarXup
import MarXup.Latex
import MarXup.Tex
import MarXup.MultiRef 
import MarXup.Diagram.Tikz (showDistance)
import Data.Foldable (forM_)

type BoxTex = (TeX,BoxSpec)

-- | Initial document
data DOC = NIL
         | DOC :<> DOC
         | TEXT BoxTex
         | NEST Double DOC
         | LINE
         | DOC :<|> DOC

-- | Document "process"
data Doc = Nil
         | Text BoxTex Doc
         | Line Double Doc -- New line at a given indentation

instance Monoid DOC where
  mempty = NIL
  mappend = (:<>)

-- | Add the alternative to represent everything on one line
group :: DOC -> DOC
group x = flatten x :<|> x

space = TEXT (hspace "4pt",BoxSpec {boxWidth = 4, boxHeight = 0, boxDepth = 0})

-- | Make the document fit on one line.
flatten :: DOC -> DOC
flatten NIL         = NIL
flatten LINE        = space
flatten (x :<> y)   = flatten x :<> flatten y
flatten (NEST _i x) = flatten x
flatten (TEXT s)    = TEXT s
flatten (x :<|> _y) = flatten x

len = boxWidth . snd
hei (_,x) = boxHeight x + boxDepth x

better w k x y = if fits (w-k) x then x else y

-- | Does the first line of the document fit in the given width?
fits :: Double -> Doc -> Bool
fits w _ | w < 0 = False
fits _ Nil = True
fits w (s `Text` x) = fits (w - boxWidth (snd s)) x
fits _ (_ `Line` _) = True

-- | The best possible layout fitting the given width
best :: Double -> DOC -> Doc
best w x = be w 0 [(0,x)]

-- @be width k parts@ finds the bets possible layout of
-- maximum @width@, for each part @(i,d)@; a document with a given indentation.
be :: Double -> Double -> [(Double, DOC)] -> Doc
be _ _ [] = Nil
be w k ((_,NIL):z) = be w k z
be w k ((i,x :<> y):z) = be w k ((i,x):(i,y):z)
be w k ((i,NEST j x):z) = be w k ((i+j,x):z)
be w k ((_,TEXT s):z) = s `Text` be w (k + len s) z
be w _ ((i, LINE):z) = Line i (be w i z)
be w k ((i,x :<|> y):z) = better w k (be w k ((i,x):z)) (be w k ((i,y):z))

linify :: Doc -> Double -> [BoxTex] -> [(Double,[BoxTex])]
linify Nil i t = [(i,t)]
linify (Text s x) i t = linify x i (t ++ [s])
linify (Line i' x) i t = (i,t):linify x i' mempty

computeHeights :: [(Double,[BoxTex])] -> [Double]
computeHeights ls = scanl (\y (_i,boxes) -> y - maximum (map hei boxes)) 0 ls

heights ::  [(Double,[BoxTex])] -> [(Double,(Double, [BoxTex]))]
heights ls = zip (computeHeights ls) ls

computeWidths :: Double -> [BoxTex] -> [Double]
computeWidths = scanl (\x b -> len b + x)

layout :: Doc -> Tex ()
layout d = env "tikzpicture" $ do
  forM_ (heights $ linify d 0 mempty) $ \(y,(i,boxes)) -> do
    let mh = maximum (map (boxHeight . snd) boxes)
    forM_ (zip boxes $ computeWidths i boxes) $ \((t,box),x) -> do
        let y' = y - (mh - boxHeight box)
        tex $ "\\node[anchor=north west,inner sep=0] at (" ++ showDistance x ++ "," ++ showDistance y' ++ ")"
        inBox $ braces $ t
        tex ";\n"

pretty :: Double -> DOC -> TeX
pretty w d = do
  layout $ best w d

  {-
getBoxes :: DOC -> Tex DOC
getBoxes NIL = pure NIL
getBoxes (d :<> e) =  (:<>) <$> getBoxes d <*> getBoxes e
getBoxes (d :<|> e) =  (:<|>) <$> getBoxes d <*> getBoxes e
getBoxes (TEXT (t,_)) = do
  b <- justBox t
  return (TEXT (t,b))
getBoxes (NEST i d) = NEST i <$> getBoxes d
getBoxes LINE = pure LINE
-}
