{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell #-}

module MarXup.Diagram.Path where

import MarXup.Diagram.Layout
import MarXup.Diagram.Point
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
import Prelude hiding (sum,mapM_,mapM,concatMap,maximum,minimum)

freezePoint :: Point -> Diagram FrozenPoint
freezePoint (Point x y) = CB.Point <$> valueOf x <*> valueOf y

freezePath :: Path -> Diagram FrozenPath
freezePath = traverse freezePoint

toBeziers :: FrozenPath -> [CubicBezier]
toBeziers EmptyPath = []
toBeziers (Path start ss) | not (null ss) &&
                            isCycle (last ss) = filterNilCurves $ toBeziers' start (init ss ++ [StraightTo start])
                          | otherwise = filterNilCurves $ toBeziers' start ss

filterNilCurves = filter (not . isNil)

isNil :: CubicBezier -> Bool
isNil (CubicBezier a b c d) = (maxx - minx < eps) && (maxy - miny < eps)
  where xs = map CB.pointX pts
        ys = map CB.pointY pts
        minx = minimum xs
        miny = minimum ys
        maxx = maximum xs
        maxy = maximum ys
        eps = 0.001
        pts = [a,b,c,d]

toBeziers' :: FrozenPoint -> [Segment FrozenPoint] -> [CubicBezier]
toBeziers' _ [] = []
toBeziers' start (StraightTo next:ss) = CubicBezier start mid mid next : toBeziers' next ss
  where mid = avg [start, next]
toBeziers' p (CurveTo c d q:ss) = CubicBezier p c d q : toBeziers' q ss

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

