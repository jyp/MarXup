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

newtype Object = Object (Anchor -> Point)

hdiff,vdiff :: Object -> Object -> Expr
hdiff x y = xpart (y # W - x # E)
vdiff x y = ypart (y # S - x # N)

xdiff,ydiff :: Point -> Point -> Expr
xdiff p q = xpart (q - p)
ydiff p q = ypart (q - p)

extend :: Expr -> Object -> Object
extend e o = Object $ \a -> o # a + shiftInDir a e

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
  t # anchor .=. labeled
  return t

abstractPoint :: Diagram Object
abstractPoint = do
  [x,y] <- newVars (replicate 2 ContVar)
  return $ Object $ \a -> case a of
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
  return $ Object $ \anch -> case anch of
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

vrule :: Diagram Object
vrule = do
  o <- abstractBox
  align xpart [o # W, o #Center, o#E]
  return o

hrule :: Diagram Object
hrule = do
  o <- abstractBox
  height o === 0
  return o

infix 8 #
(Object o) # anch = o anch
height o = ypart (o # N - o # S)
width o = xpart (o # E - o # W)
ascent o = ypart (o # N - o # Base)
descent o = ypart (o # Base - o # S)

boundingRect :: Object -> Path
boundingRect l =
  polygon (map (l #) [NW,NE,SE,SW])
  -- polyline (map l [BaseE,BaseW])


fitsVerticallyIn :: Object -> Object -> Diagram ()
o `fitsVerticallyIn` o' = do
  let dyN = ypart $ o # N - o' # N
      dyS = ypart $ o' # S - o # S
  minimize dyN
  dyN >== 0
  minimize dyS
  dyS >== 0

fitsHorizontallyIn :: Object -> Object -> Diagram ()
o `fitsHorizontallyIn` o' = do
  let dyW = xpart $ o # W - o' # W
      dyE = xpart $ o' # E - o # E
  minimize dyW
  dyW >== 0
  minimize dyE
  dyE >== 0

rectangleObj :: Diagram Object
rectangleObj = do
  l <- abstractBox
  draw $ path $ boundingRect l
  return l

texObj :: TeX -> Diagram Object
texObj t = do
  l <- abstractBox
  BoxSpec wid h desc <- drawText (l # NW) t

  width   l === constant wid
  descent l === constant desc
  height  l === constant h
  -- drawBounds l -- for debugging
  return l

-- We'd like to also return an automatic anchor advice, as tikz does;
-- but this can't be used in constraints (circularity), unless we use
-- MIP instead of LP.
edge :: Object -> Object -> Diagram Point
edge source target = do
  let points = [source # Center,target # Center]
      link = polyline points
      targetArea = boundingRect $ extend 3 target
      sourceArea = boundingRect $ extend 3 source
  l' <- freezePath link
  sa' <- freezePath sourceArea
  ta' <- freezePath targetArea
  frozenPath $ (l' `cutAfter` ta') `cutBefore` sa'
  return $ avg points


