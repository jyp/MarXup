{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell #-}

module MarXup.Diagram.Geometry (module MarXup.Diagram.Geometry) where

import MarXup.Diagram.Layout
import Data.Traversable
import Data.Foldable
import Data.Algebra
import qualified Geom2D.CubicBezier as CB
import Geom2D.CubicBezier (CubicBezier(..))
import Control.Applicative
import Data.List (sort,transpose)
import Data.Maybe (listToMaybe)
-- import Data.Traversable
-- import Data.Foldable
import Prelude hiding (sum,mapM_,mapM,concatMap)

infix 4 .=.
----------------
-- Points 
-- | A point in 2d space
data Point = Point {xpart :: Expr, ypart :: Expr}
  deriving (Eq,Show)
           
instance Num Point where
  negate = neg
  (+) = (^+^)
  (-) = (^-^)

instance Group Point where
  zero = Point zero zero
  Point x1 y1 ^+^ Point x2 y2 = Point (x1 ^+^ x2) (y1 ^+^ y2)
  neg (Point x y) = Point (neg x) (neg y)

instance Module Constant Point where
  k *^ Point x y = Point (k *^ x) (k *^ y)

-- | Orthogonal norm of a vector
orthonorm :: Point -> Diagram Expr
orthonorm (Point x y) =
  (+) <$> absoluteValue x <*> absoluteValue y

-- | Orthogonal distance between points.
orthoDist :: Point -> Point -> Diagram Expr
orthoDist p q = orthonorm (q-p)

-- | Rotate a vector 90 degres in the trigonometric direction.
rotate90, rotate180 :: Point -> Point
rotate90 (Point x y) = Point (negate y) x

rotate180 = rotate90 . rotate90

xdiff,ydiff :: Point -> Point -> Expr
xdiff p q = xpart (q - p)
ydiff p q = ypart (q - p)

-----------------
-- Point constraints

(.=.),northOf,southOf,westOf,eastOf :: Point -> Point -> Diagram ()
Point x1 y1 .=. Point x2 y2 = do
  x1 === x2
  y1 === y2

northOf (Point _ y1) (Point _ y2) = y2 <== y1
southOf = flip northOf
westOf (Point x1 _) (Point x2 _) = x1 <== x2
eastOf = flip westOf

alignHoriz,alignVert :: [Point] -> Diagram ()
alignHoriz = align ypart
alignVert = align xpart

align :: (a -> Expr) -> [a] -> Diagram ()
align _ [] = return ()
align f (p:ps) = forM_ ps $ \p' -> f p === f p'

alignMatrix :: [[Point]] -> Dia
alignMatrix ls = do
  forM_ ls alignHoriz
  forM_ (transpose ls) alignVert

---------------------
-- Point objectives

southwards, northwards, westwards, eastwards :: Point -> Diagram ()
southwards (Point _ y) = minimize y
westwards (Point x _) = minimize x
northwards = southwards . negate
eastwards = westwards . negate


freezePoint :: Point -> Diagram FrozenPoint
freezePoint (Point x y) = CB.Point <$> valueOf x <*> valueOf y

freezePath :: Path -> Diagram FrozenPath
freezePath = traverse freezePoint

toBeziers :: FrozenPath -> [CubicBezier]
toBeziers EmptyPath = []
toBeziers (Path start ss) | not (null ss) &&
                            isCycle (last ss) = toBeziers' start (init ss ++ [StraightTo start])
                          | otherwise = toBeziers' start ss

fromBeziers :: [CubicBezier] -> FrozenPath 
fromBeziers [] = EmptyPath
fromBeziers (CubicBezier p c d q:bs) = Path p (CurveTo c d q:pathSegments (fromBeziers bs))

pathSegments :: Path' t -> [Segment t]
pathSegments EmptyPath = []
pathSegments (Path _ ss) = ss

isCycle Cycle = True
isCycle _  = False

type FrozenPoint = CB.Point

frozenPointElim (CB.Point x y) f = f x y

instance Group FrozenPoint where
  zero = CB.Point 0 0
  (^+^) = (CB.^+^)
  (^-^) = (CB.^-^)

instance Module Constant FrozenPoint where
  (*^) = (CB.*^)

toBeziers' :: FrozenPoint -> [Segment FrozenPoint] -> [CubicBezier]
toBeziers' _ [] = []
toBeziers' start (StraightTo next:ss) = CubicBezier start mid mid next : toBeziers' next ss
  where mid = avg [start, next]
toBeziers' p (CurveTo c d q:ss) = CubicBezier p c d q : toBeziers' q ss

clipOne :: CubicBezier -> [CubicBezier] -> Maybe CubicBezier
clipOne b cutter = fmap firstPart $ listToMaybe $ sort $ concatMap (\b' -> map fst $ CB.bezierIntersection b b' 0.001) cutter
  where firstPart t = fst $ CB.splitBezier b t

-- | @cutAfter path area@ cuts the path after its first intersection with the @area@.
cutAfter', cutBefore' :: [CubicBezier] -> [CubicBezier] -> [CubicBezier]
cutAfter' [] _cutter = []
cutAfter' (b:bs) cutter = case clipOne b cutter of
  Nothing -> b:cutAfter' bs cutter
  Just b' -> [b']

revBeziers :: [CubicBezier] -> [CubicBezier]
revBeziers = reverse . map rev
  where rev (CubicBezier a b c d) = CubicBezier d c b a

cutBefore' path area = revBeziers $ cutAfter' (revBeziers path) area

onBeziers :: ([CubicBezier] -> [CubicBezier] -> [CubicBezier])
             -> FrozenPath -> FrozenPath -> FrozenPath
onBeziers op p' q' = fromBeziers $ op (toBeziers p') (toBeziers q')

type FrozenPath = Path' FrozenPoint

cutAfter :: FrozenPath -> FrozenPath -> FrozenPath
cutAfter = onBeziers cutAfter'

cutBefore :: FrozenPath -> FrozenPath -> FrozenPath
cutBefore = onBeziers cutBefore'

data Segment point = CurveTo point point point
                   | StraightTo point
                   | Cycle
                   -- | Rounded (Maybe Constant)
                   -- | HV point | VH point
  deriving (Show,Eq)
instance Functor Segment where
  fmap = fmapDefault
  
instance Foldable Segment where
  foldMap = foldMapDefault
instance Traversable Segment where
  traverse _ Cycle = pure Cycle
  traverse f (StraightTo p) = StraightTo <$> f p
  traverse f (CurveTo c d q) = CurveTo <$> f c <*> f d <*> f q
  

-----------------
-- Paths

type Path = Path' Point

data Path' a
  = EmptyPath
  | Path {startingPoint :: a
         ,segments :: [Segment a]}
  deriving Show
           
instance Functor Path' where
  fmap = fmapDefault

instance Foldable Path' where
  foldMap = foldMapDefault
instance Traversable Path' where
  traverse _ EmptyPath = pure EmptyPath
  traverse f (Path s ss) = Path <$> f s <*> traverse (traverse f) ss


polyline :: [Point] -> Path
polyline [] = EmptyPath
polyline (x:xs) = Path x (map StraightTo xs)

polygon :: [Point] -> Path
polygon [] = EmptyPath
polygon (x:xs) = Path x (map StraightTo xs ++ [Cycle])

-- | Circle approximated with 4 curves
circle :: Point -> Expr -> Path
circle center r = (center ^+^) <$>
                       Path (Point r 0)
                         [CurveTo (Point r k) (Point k r) (Point 0 r),
                          CurveTo (Point (-k) r) (Point (-r) k) (Point (-r) 0),
                          CurveTo (Point (-r) (-k)) (Point (-k) (-r)) (Point 0 (-r)),
                          CurveTo (Point k (-r)) (Point r (-k)) (Point r 0),
                          Cycle]
 where k1 :: Constant
       k1 = 4 * (sqrt 2 - 1) / 3
       k = k1 *^ r

