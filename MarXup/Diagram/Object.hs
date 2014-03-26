{-# LANGUAGE DataKinds, KindSignatures, OverloadedStrings, EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, OverlappingInstances   #-}

module MarXup.Diagram.Object where

import MarXup
import MarXup.Tex
import MarXup.Tikz
import MarXup.Diagram.Geometry
import MarXup.Diagram.Layout
import MarXup.MultiRef (BoxSpec(..))
import Control.Monad
import Control.Applicative
import Data.Algebra
import Data.List (intersperse)

data Anchor = Center | N | NW | W | SW | S | SE | E | NE | BaseW | Base | BaseE
  deriving Show

-- | Box-shaped object. (a subtype)
type Box = Object

newtype Anchorage = Anchorage {boxAnchors :: Anchor -> Point}
data Object = Object {objectOutline :: Path, objectAnchorage :: Anchorage}

class Anchored a where
  anchors :: a -> Anchor -> Point
infix 8 #

(#) :: Anchored a => a -> Anchor -> Point
(#) = anchors

instance Anchored Anchorage where
  anchors = boxAnchors

instance Anchored Object where
  anchors = anchors . objectAnchorage

instance Anchored Point where
  anchors p _ = p

-- | Horizontal distance between objects
hdist :: Anchored a => a -> a -> Expr
hdist x y = xpart (y # W - x # E)

-- | Vertical distance between objects
vdist :: Anchored a => a -> a -> Expr
vdist x y = ypart (y # S - x # N)

-- | Extend the box boundaries by the given delta
extend :: Expr -> Anchorage -> Anchorage
extend e o = Anchorage $ \a -> o # a + shiftInDir a e

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

-- | Make a label object. This is just some text surrounded by 4
-- points of blank.
mkLabel :: TeX -> Diagram Anchorage
mkLabel texCode = extend 4 <$> texBox texCode

labelObj :: TeX -> Diagram Box
labelObj = rectangleShape <=< mkLabel

-- | Label a point by a given TeX expression, at the given anchor.
labelPt :: TeX -> Anchor -> Point -> Diagram Box
labelPt labell anchor labeled  = do
  t <- labelObj labell 
  t # anchor .=. labeled
  return t

-- | A free point
point :: Diagram Point
point = do
  [x,y] <- newVars (replicate 2 ContVar)
  return $ Point x y

-- | A point anchorage (similar to a box of zero width and height)
pointBox :: Diagram Anchorage
pointBox = do
  p <- point
  return $ Anchorage $ \a -> case a of _ -> p

-- | A box. Anchors are aligned along a grid.
box :: Diagram Anchorage
box = do
  [n,s,e,w,base,midx,midy] <- newVars (replicate 7 ContVar)
  n >== base
  base >== s
  w <== e
  
  midx === avg [w,e]
  midy === avg [n,s]
  let pt = flip Point
  return $ Anchorage $ \anch -> case anch of
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

-- | A box of zero width
vrule :: Diagram Anchorage
vrule = do
  o <- box
  align xpart [o # W, o #Center, o#E]
  return o

-- | A box of zero height
hrule :: Diagram Anchorage
hrule = do
  o <- box
  height o === 0
  return o


height o = ypart (o # N - o # S)
width o = xpart (o # E - o # W)
ascent o = ypart (o # N - o # Base)
descent o = ypart (o # Base - o # S)

-- fitsVerticallyIn :: Anchorage -> Anchorage -> Diagram ()
o `fitsVerticallyIn` o' = do
  let dyN = ypart $ o' # N - o # N
      dyS = ypart $ o # S - o' # S
  minimize dyN
  dyN >== 0
  minimize dyS
  dyS >== 0

-- fitsHorizontallyIn :: Anchorage -> Anchorage -> Diagram ()
o `fitsHorizontallyIn` o' = do
  let dyW = xpart $ o # W - o' # W
      dyE = xpart $ o' # E - o # E
  minimize dyW
  dyW >== 0
  minimize dyE
  dyE >== 0

a `fitsIn` b = do
  a `fitsHorizontallyIn` b
  a `fitsVerticallyIn` b

circleShape :: Diagram Object
circleShape = do
  anch <- box
  width anch === height anch
  let radius = 0.5 *- width anch
  let p = circle (anch # Center) radius
  path p
  return $ Object p anch
--   let k1 :: Constant
--       k1 = sqrt 2 / 2
--       k = k1 *^ r
--       p = circle center r
--   return $ Object p $ Anchorage $ \a -> center + case a of
--     N -> Point 0 r
--     S -> Point 0 (-r)
--     E -> Point r 0
--     W -> Point (-r) 0
--     Center -> Point 0 0
--     NE -> Point k k

rectangleShape :: Anchorage -> Diagram Object
rectangleShape l = do
  let p = polygon (map (l #) [NW,NE,SE,SW])
  path p
  return $ Object p l

traceAnchorage :: Color -> Object -> Diagram ()
traceAnchorage c l = do
  stroke c $ path $ polygon (map (l #) [NW,NE,SE,SW])
  -- TODO: draw the baseline, etc.

-- | Typeset a piece of text and return its bounding box.
texBox :: TeX -> Diagram Anchorage
texBox t = do
  l <- box
  BoxSpec wid h desc <- drawText (l # NW) t

  width   l === constant wid
  descent l === constant desc
  height  l === constant h
  return l

data Incidence = Incidence { incidencePoint, incidenceNormal :: Point }
swap :: Incidence -> Incidence
swap (Incidence p v) = Incidence p (negate v)

-- | Traces a straight edge between two objects.
-- The midpoint is returned, as well as a normal vector.
edge :: Object -> Object -> Diagram Incidence
edge source target = do
  let points@[a,b] = [source # Center,target # Center]
      link = polyline points
      targetArea = objectOutline target
      sourceArea = objectOutline source
  l' <- freezePath link
  sa' <- freezePath sourceArea
  ta' <- freezePath targetArea
  frozenPath $ (l' `cutAfter` ta') `cutBefore` sa'
  return $ Incidence (avg points) (rotate90 (b-a))

(.<.) :: Point -> Point -> Diagram ()
Point x1 y1 .<. Point x2 y2 = do
  x1 <== x2
  y1 <== y2

-- | Forces the point to be inside the (bounding box) of the object.
inside :: Point -> Box -> Diagram ()
inside p o = do
  (o # SW) .<. p
  p .<. (o # NE)

-- | @autoLabel label i@ Layouts the label at the given incidence
-- point.
autoLabel :: Box -> Incidence -> Diagram ()
autoLabel lab (Incidence pt norm) = do
  pt `inside` lab
  minimize =<< orthoDist (lab#Center) (pt + norm)

-- | @labeledEdge label source target@
labeledEdge :: Object -> Object -> Box -> Diagram ()
labeledEdge source target lab = autoLabel lab =<< edge source target

