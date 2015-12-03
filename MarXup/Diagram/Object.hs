{-# LANGUAGE DataKinds, KindSignatures, OverloadedStrings, EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, GADTs, LambdaCase   #-}

module MarXup.Diagram.Object where

-- import MarXup
-- import MarXup.Tex
import MarXup.Diagram.Path
import MarXup.Diagram.Point
import MarXup.Diagram.Layout
import Control.Monad
-- import Control.Applicative
-- import Data.Algebra
-- import Data.List (intersperse)
import Control.Lens (set,view)

data Anchor = Center | N | NW | W | SW | S | SE | E | NE | BaseW | Base | BaseE
  deriving Show

-- | Box-shaped object. (a subtype)
type Box = Object

newtype Anchorage = Anchorage {fromAnchorage :: Anchor -> Point}
data Object = Object {objectOutline :: Path, objectAnchorage :: Anchorage}

class Anchored a where
  anchors :: a -> Anchorage
  
infix 8 #

(#) :: Anchored a => a -> Anchor -> Point
(#) = fromAnchorage . anchors

instance Anchored Anchorage where
  anchors = id

instance Anchored Object where
  anchors = objectAnchorage

instance Anchored Point where
  anchors p = Anchorage $ \_ -> p

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
shiftInDir BaseW d = negate d `Point` 0
shiftInDir E d  = d `Point` 0
shiftInDir BaseE d  = d `Point` 0
shiftInDir NW d = negate d `Point` d
shiftInDir SE d = d `Point` negate d
shiftInDir SW d = negate d `Point` negate d
shiftInDir NE d = d `Point` d
shiftInDir _ _  = 0 `Point` 0

-- | Make a label object. This is just some text surrounded by 4
-- points of blank.
mkLabel :: Monad m => m () -> Diagram m Anchorage
mkLabel texCode = extend 4 <$> texBox texCode

labelObj :: Monad m => m () -> Diagram m Box
labelObj = rectangleShape <=< mkLabel

-- | Label a point by a given TeX expression, at the given anchor.
labelPt :: Monad m => m () -> Anchor -> Point -> Diagram m Box
labelPt labell anchor labeled  = do
  t <- labelObj labell 
  t # anchor .=. labeled
  return t

-- | A free point
point :: Monad m => Diagram m Point
point = do
  [x,y] <- newVars (replicate 2 ContVar)
  return $ Point x y

-- | A point anchorage (similar to a box of zero width and height)
pointBox :: Monad m => Diagram m Anchorage
pointBox = anchors <$> point

-- | A box. Anchors are aligned along a grid.
box :: Monad m => Diagram m Anchorage
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
vrule :: Monad m => Diagram m Anchorage
vrule = do
  o <- box
  align xpart [o # W, o #Center, o#E]
  return o

-- | A box of zero height
hrule :: Monad m => Diagram m Anchorage
hrule = do
  o <- box
  height o === 0
  return o

height, width, ascent, descent :: Anchored a => a -> Expr
height o = ypart (o # N - o # S)
width o = xpart (o # E - o # W)
ascent o = ypart (o # N - o # Base)
descent o = ypart (o # Base - o # S)

-- | Make one object fit (snugly) in the other.
fitsIn, fitsHorizontallyIn, fitsVerticallyIn :: (Monad m, Anchored a, Anchored b) => a -> b -> Diagram m ()
o `fitsVerticallyIn` o' = do
  let dyN = ypart $ o' # N - o # N
      dyS = ypart $ o # S - o' # S
  minimize dyN
  dyN >== 0
  minimize dyS
  dyS >== 0

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

-- | A circle
circleShape :: Monad m => Diagram m Object
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

rectangleShape :: Monad m => Anchorage -> Diagram m Object
rectangleShape l = do
  let p = polygon (map (l #) [NW,NE,SE,SW])
  path p
  return $ Object p l

traceAnchorage :: (Anchored a, Monad m) => Color -> a -> Diagram m ()
traceAnchorage c l = do
  stroke c $ path $ polygon (map (l #) [NW,NE,SE,SW])
  -- TODO: draw the baseline, etc.

-- | Typeset a piece of text and return its bounding box.
texBox :: Monad m => m () -> Diagram m Anchorage
texBox t = do
  l <- box
  -- traceAnchorage "red" l
  BoxSpec wid h desc <- drawText (l # NW) t

  width   l === constant wid
  descent l === constant desc
  height  l === constant (h + desc)
  return l

-- | A vector with an origin
data OVector = OVector { vectorOrigin, vectorMagnitude :: Point }

-- | Turn the orientation by 180 degrees
turn180 :: OVector -> OVector
turn180 (OVector p v) = OVector p (negate v)

data FList xs a where
  NIL :: FList '[] a
  (:%>) :: Functor t => t a -> FList fs a -> FList ('(:) t fs) a

infixr :%>

instance Functor (FList xs) where
  fmap _ NIL = NIL
  fmap f (x :%> xs) = fmap f x :%> fmap f xs
  
-- | Traces a straight edge between two objects.
-- A vector originated at the midpoint and pointing perpendicular to
-- the edge is returned.
edge :: Monad m => Object -> Object -> Diagram m OVector
edge source target = do
  let points@[a,b] = [source # Center,target # Center]
      link = polyline points
      targetArea = objectOutline target
      sourceArea = objectOutline source
  options <- view diaPathOptions
  tracePath' <- view (diaBackend . tracePath)
  freeze (link :%> sourceArea :%> targetArea :%> NIL) $ \(l' :%> sa' :%> ta' :%> NIL) -> do
       tracePath' options $ (l' `cutAfter` ta') `cutBefore` sa'
  return $ OVector (avg points) (rotate90 (b-a))

(.<.) :: Monad m => Point -> Point -> Diagram m ()
Point x1 y1 .<. Point x2 y2 = do
  x1 <== x2
  y1 <== y2

-- | Forces the point to be inside the (bounding box) of the object.
insideBox :: Monad m => Anchored a => Point -> a -> Diagram m ()
insideBox p o = do
  (o # SW) .<. p
  p .<. (o # NE)

-- | @autoLabel label i@ Layouts the label at the given incidence
-- vector.
autoLabel :: Monad m => Box -> OVector -> Diagram m ()
autoLabel lab (OVector pt norm) = do
  pt `insideBox` lab
  minimize =<< orthoDist (lab#Center) (pt + norm)

-- | @labeledEdge label source target@
labeledEdge :: Monad m => Object -> Object -> Box -> Diagram m ()
labeledEdge source target lab = autoLabel lab =<< edge source target



-------------------
-- Even higher-level primitives:

nodeDistance :: Expr
nodeDistance = 5

leftOf :: Monad m => Object -> Object -> Diagram m ()
a `leftOf` b = spread hdist nodeDistance [a,b]

topOf :: Monad m => Object -> Object -> Diagram m ()
a `topOf` b =  spread vdist nodeDistance [b,a]

-- | Spread a number of objects by *minimum* a given distance. example: @spread
-- hdist 30 ps@
spread :: Monad m => (t -> t -> Expr) -> Expr -> [t] -> Diagram m ()
spread f d (x:y:xs) = do
  f x y >== d
  minimize $ f x y
  spread f d (y:xs)
spread _ _ _ = return ()

-- | A node: a labeled circle
node :: Monad m => m () -> Diagram m Object
node lab = do
  l <- extend 4 <$> texBox lab
  c <- draw $ circleShape
  l `fitsIn` c
  l # Center .=. c # Center
  return c

-- | Draw an arrow between two objects
arrow :: Monad m => Object -> Object -> Diagram m OVector
arrow src trg = using (outline "black" . set endTip LatexTip) $ do
  edge src trg

-- | Bounding box of a number of anchored values
boundingBox :: (Monad m, Anchored a) => [a] -> Diagram m Object
boundingBox os = do
  bx <- box
  mapM_ (`fitsIn` bx) os
  rectangleShape bx
