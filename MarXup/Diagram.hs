{-# LANGUAGE DataKinds, KindSignatures, OverloadedStrings, EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, OverlappingInstances   #-}

module MarXup.Diagram where

import Data.LinearProgram.Common
import MarXup
import MarXup.Tex
import MarXup.Tikz
import MarXup.MultiRef (BoxSpec(..))
import Control.Monad
import Control.Applicative
import Data.List (intersperse)

data Anchor = Center | N | NW | W | SW | S | SE | E | NE | BaseW | Base | BaseE
  deriving Show

type Object = Anchor -> Point

hdist,vdist :: Object -> Object -> Expr
hdist x y = xpart (y W - x E)
vdist x y = ypart (y S - x N)

{-
boundingZone :: Expr ObjectRef -> Expr Path
boundingZone l = foldr (...) closed $ map (\a -> shiftedAnchor 2 a l) [NW,NE,SE,SW]

-- line :: (Expr Path -> [Expr DrawOption] -> MP ()) -> [Expr DrawOption] -> Expr ObjectRef -> Expr ObjectRef -> MP (Expr Pair)
-- line renderer opts source target = do
--   c <- mkRef "z" <$> mpLabel
--   mpRaw "pair " <> out c <>";\n"
--   delay $ renderer (Center ▸ source ... open (Center ▸ target))
--     (opts ++ [cutAfter $ boundingZone target,
--               cutBefore $ boundingZone source])
--   c === barycenter [Center ▸ source,Center ▸ target]
--   return c

-- | Makes a shift of size 'd' in the given direction.
-- TODO: divide d by sqrt 2 for the diagonals
shiftInDir :: Anchor -> Expr -> Point
shiftInDir N d = 0 +: d
shiftInDir S d = 0 +: negate d
shiftInDir W d = negate d +: 0
shiftInDir E d = d +: 0
shiftInDir NW d = negate d +: d
shiftInDir SE d = d +: negate d
shiftInDir SW d = negate d +: negate d
shiftInDir NE d = d +: d
shiftInDir _ _ = 0 +: 0

-- | Label a point by a given TeX expression, at the given anchor.
labelPt :: TeX -> Anchor -> Expr Pair -> MP (Expr ObjectRef)
labelPt labell anchor labeled  = do
  t <- textObj labell
  shiftedAnchor 4 anchor t === labeled
  return t

shiftedAnchor delta anchor obj = shiftInDir anchor delta + (anchor ▸ obj)
-}


abstractBox :: Diagram Object
abstractBox = do
  [n,s,e,w,base,midx,midy] <- newVars (replicate 7 ContVar)
  midx === avg [w,e]
  midy === avg [n,s]
  let pt = flip Point
  return $ \anch -> case anch of
    NW     -> pt n    w
    N      -> pt n    midx
    NE     -> pt n    e  
    E      -> pt midy e
    SE     -> pt s    e
    S      -> pt s    midx
    SW     -> pt s    w
    W      -> pt midy w
    Center -> pt midy midx
    Base   -> pt base midx
    BaseE  -> pt base e
    BaseW  -> pt base w

anch ▸ o = o anch
height o = ypart (N ▸ o - S ▸ o)
width o = xpart (E ▸ o - W ▸ o)
ascent o = ypart (N ▸ o - Base ▸ o)
descent o = ypart (Base ▸ o - S ▸ o)

drawBounds :: Object -> Diagram ()
drawBounds l = do
  drawPoly (map l [NW,NE,SE,SW])
  -- drawLine (map l [N,S])
  drawLine (map l [BaseE,BaseW])

drawPoint :: Point -> Diagram ()
drawPoint p = do
  diaRaw $ "\\circle "
  element p
  diaRaw "cycle ;\n"

drawPoly :: [Point] -> Diagram ()
drawPoly ps = do
  diaRaw $ "\\draw "
  forM ps $ \p -> do
    element p
    diaRaw "--"
  diaRaw "cycle ;\n"

drawLine :: [Point] -> Diagram ()
drawLine ps = do
  let ps' = map element ps
  diaRaw "\\draw "
  sequence_ $ intersperse (diaRaw "--") ps'
  diaRaw ";\n"

taller :: Object -> Object -> Diagram ()
taller o o' = do
  o N `northOf` o' N
  o S `southOf` o' S

smallest :: Object -> Diagram ()
smallest o = do
  southwards $ o N
  northwards $ o S

wider :: Object -> Object -> Diagram ()
wider o o' = do
  o W `westOf` o' W
  o E `eastOf` o' E
  
thinest :: Object -> Diagram ()
thinest o = do
  eastwards $ o W
  westwards $ o E

chainBases :: Expr -> [Object] -> Diagram Object
chainBases _ [] = abstractBox
chainBases spacing ls = do
  group <- abstractBox
  align ypart $ map ($ Base) ls
  forM_ (zip ls (tail ls)) $ \(x,y) ->
    westOf (x E + Point spacing 0) (y W)
  forM_ ls $ \l -> group `taller` l
  smallest group
  group W .=. head ls W
  group E .=. last ls E
  group Base .=. head ls Base
  return group

{-
boxObj :: Diagram Object
boxObj = do
  l <- abstractBox
  drawBounds l
  return l
-}

texObj :: TeX -> Diagram Object
texObj t = do
  l <- abstractBox
  BoxSpec wid h desc <- drawText (l NW) t

  width   l === constant wid
  descent l === constant desc
  height  l === constant h
  drawBounds l -- for debugging
  return l

infix 8 ▸

  
