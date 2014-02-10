{-# LANGUAGE DataKinds, KindSignatures, OverloadedStrings, EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, OverlappingInstances   #-}

module MarXup.Diagram where

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

extend :: Expr -> Object -> Object
extend e o = \a -> o a + shiftInDir a e

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
shiftInDir :: Anchor -> Expr -> Point
shiftInDir N d = 0 `Point` d
shiftInDir S d = 0 `Point` negate d
shiftInDir W d = negate d `Point` 0
shiftInDir E d  = d `Point` 0
shiftInDir NW d = negate d `Point` d
shiftInDir SE d = d `Point` negate d
shiftInDir SW d = negate d `Point` negate d
shiftInDir NE d = d `Point` d
shiftInDir _ _  = 0 `Point` 0


-- | Label a point by a given TeX expression, at the given anchor.
labelPt :: TeX -> Anchor -> Point -> Diagram Object
labelPt labell anchor labeled  = do
  t <- extend 4 <$> texObj labell
  t anchor .=. labeled
  return t

abstractPoint :: Diagram Object
abstractPoint = do
  [x,y] <- newVars (replicate 2 ContVar)
  return $ \a -> case a of
    _ -> Point x y


abstractBox :: Diagram Object
abstractBox = do
  [n,s,e,w,base,midx,midy] <- newVars (replicate 7 ContVar)
  n >== base
  base >== s
  w <== e
  
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

boundingRect :: Object -> Path
boundingRect l = 
  polygon (map l [NW,NE,SE,SW])
  -- polyline (map l [BaseE,BaseW])

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


rectangleObj :: Diagram Object
rectangleObj = do
  l <- abstractBox
  draw $ boundingRect l
  return l

texObj :: TeX -> Diagram Object
texObj t = do
  l <- abstractBox
  BoxSpec wid h desc <- drawText (l NW) t

  width   l === constant wid
  descent l === constant desc
  height  l === constant h
  -- drawBounds l -- for debugging
  return l

infix 8 ▸

  
