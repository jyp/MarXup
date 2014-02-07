{-# LANGUAGE DataKinds, KindSignatures, OverloadedStrings, EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, OverlappingInstances   #-}

module MarXup.Diagram where

import MarXup.Tex
import MarXup.Tikz
import MarXup.MultiRef (Label,Multi(Raw))
import Data.Monoid
import Control.Applicative
import Data.List (intersperse)
import Data.Char (ord,chr)
import Numeric (showIntAtBase)

data Anchor = Center | N | NW | W | SW | S | SE | E | NE | BaseW | Base | BaseE
  deriving Show

type Object = Anchor -> Point

hdist,vdist :: Object -> Object -> Expr Numeric
hdist x y = xpart (y W - x E)
vdist x y = ypart (y S - x N)

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

abstractBox :: D Object
abstractBox = do
  [n,s,e,w,base,midx,midy] <- newVars 7
  assert $ midx === avg [w,e]
  assert $ midy === avg [n,s]
  let pt = flip point
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

drawBounds :: Object -> D ()
drawBounds l = draw (NW ▸ l .-- NE ▸ l .-- SE ▸ l .-- SW ▸ l .-- closed) []

boxObj :: D Object
boxObj = do
  l <- abstractBox
  drawBounds l
  return l

textObj :: TeX -> D Object
textObj t = do
  l <- abstractBox
  BoxSpec wid asc desc <- drawText (l NW) t

  width o === wid
  descent o === desc
  ascent o === asc

  return l

infix 8 ▸
