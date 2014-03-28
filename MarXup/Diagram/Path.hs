{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell #-}

module MarXup.Diagram.Path where

import MarXup.Diagram.Layout
import MarXup.Diagram.Point
import Data.Traversable
import Data.Foldable
import Data.Algebra
-- import Data.Traversable
-- import Data.Foldable
import Graphics.Typography.Geometry.Bezier
import Control.Applicative
import Data.List (sort,transpose)
import Data.Maybe (listToMaybe)
import Prelude hiding (sum,mapM_,mapM,concatMap,maximum,minimum)
import qualified Data.Vector.Unboxed as V
import Algebra.Polynomials.Bernstein

type FrozenPoint = Point' Constant

freezePoint :: Point -> Diagram FrozenPoint
freezePoint = traverse valueOf

freezePath :: Path -> Diagram FrozenPath
freezePath = traverse freezePoint

toBeziers :: FrozenPath -> [Curve]
toBeziers EmptyPath = []
toBeziers (Path start ss) | not (null ss) &&
                            isCycle (last ss) = toBeziers' start (init ss ++ [StraightTo start])
                          | otherwise = toBeziers' start ss

-- filterNilCurves = filter (not . isNil)

-- isNil :: Curve -> Bool
-- isNil (Curve a b c d) = (maxx - minx < eps) && (maxy - miny < eps)
--   where xs = map CB.pointX pts
--         ys = map CB.pointY pts
--         minx = minimum xs
--         miny = minimum ys
--         maxx = maximum xs
--         maxy = maximum ys
--         eps = 0.001
--         pts = [a,b,c,d]

curve (Point xa ya) (Point xb yb) (Point xc yc) (Point xd yd) = bezier3 xa ya xb yb xc yc xd yd

toBeziers' :: FrozenPoint -> [Segment FrozenPoint] -> [Curve]
toBeziers' _ [] = []
toBeziers' start (StraightTo next:ss) = curve start mid mid next : toBeziers' next ss
  where mid = avg [start, next]
toBeziers' p (CurveTo c d q:ss) = curve p c d q : toBeziers' q ss

fromBeziers :: [Curve] -> FrozenPath 
fromBeziers [] = EmptyPath
fromBeziers (Bezier cx cy t0 t1:bs) = Path p (CurveTo c d q:pathSegments (fromBeziers bs))
  where [cx',cy'] = map (\c -> coefs $ restriction c t0 t1) [cx,cy]
        cxy = V.zip cx' cy'
        [p,c,d,q] = map toPt $ V.foldr (:) [] cxy
        toPt (x,y) = Point x y

pathSegments :: Path' t -> [Segment t]
pathSegments EmptyPath = []
pathSegments (Path _ ss) = ss

isCycle Cycle = True
isCycle _  = False

frozenPointElim (Point x y) f = f x y

splitBezier (Bezier cx cy t0 t1) (u,v,_,_) = (Bezier cx cy t0 u, Bezier cx cy v t1)

clipOne :: Curve -> [Curve] -> Maybe Curve
clipOne b cutter = fmap firstPart $ listToMaybe $ sort $ concatMap (inter b) cutter
  where firstPart t = fst $ splitBezier b t

-- | @cutAfter path area@ cuts the path after its first intersection with the @area@.
cutAfter', cutBefore' :: [Curve] -> [Curve] -> [Curve]
cutAfter' [] _cutter = []
cutAfter' (b:bs) cutter = case clipOne b cutter of
  Nothing -> b:cutAfter' bs cutter
  Just b' -> [b']

revBernstein (Bernsteinp n c) = Bernsteinp n (V.reverse c)
revBeziers :: [Curve] -> [Curve]
revBeziers = reverse . map rev
  where rev (Bezier cx cy t0 t1) = (Bezier (revBernstein cx) (revBernstein cy) (1-t1) (1-t0))

cutBefore' path area = revBeziers $ cutAfter' (revBeziers path) area

onBeziers :: ([Curve] -> [Curve] -> [Curve])
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


-- | Circle approximated with 4 cubic bezier curves
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

