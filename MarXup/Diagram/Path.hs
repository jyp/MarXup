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
import Graphics.Typography.Geometry.Bezier as MarXup.Diagram.Point (Curve) 
import Control.Applicative
import Data.List (sort,transpose)
import Data.Maybe (listToMaybe)
import Prelude hiding (sum,mapM_,mapM,concatMap,maximum,minimum)
import qualified Data.Vector.Unboxed as V
import Algebra.Polynomials.Bernstein (restriction,Bernsteinp(..))
import Control.Lens (over, set, view)
import Control.Monad.Reader (local)

unfreeze :: Functor t => t Constant -> t Expr
unfreeze = fmap constant

toBeziers :: FrozenPath -> [Curve]
toBeziers EmptyPath = []
toBeziers (Path start ss) | not (null ss) &&
                            isCycle (last ss) = toBeziers' start (init ss ++ [StraightTo start])
                          | otherwise = toBeziers' start ss

curveSegment (Point xa ya) (Point xb yb) (Point xc yc) (Point xd yd) = bezier3 xa ya xb yb xc yc xd yd
lineSegment (Point xa ya) (Point xb yb) = line xa ya xb yb

toBeziers' :: FrozenPoint -> [Frozen Segment] -> [Curve]
toBeziers' _ [] = []
toBeziers' start (StraightTo next:ss) = curveSegment start mid mid next : toBeziers' next ss
  where mid = avg [start, next]
toBeziers' p (CurveTo c d q:ss) = curveSegment p c d q : toBeziers' q ss

fromBeziers :: [Curve] -> FrozenPath
fromBeziers [] = EmptyPath
fromBeziers (Bezier cx cy t0 t1:bs) = case map toPt $ V.foldr (:) [] cxy of
      [p,c,d,q] -> Path p (CurveTo c d q:rest)
      [p,q] -> Path p (StraightTo q:rest)
  where [cx',cy'] = map (\c -> coefs $ restriction c t0 t1) [cx,cy]
        cxy = V.zip cx' cy'
        toPt (x,y) = Point x y
        rest = pathSegments (fromBeziers bs)

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


cutAfter :: FrozenPath -> FrozenPath -> FrozenPath
cutAfter = onBeziers cutAfter'

cutBefore :: FrozenPath -> FrozenPath -> FrozenPath
cutBefore = onBeziers cutBefore'

-----------------
-- Paths


type Path = Path' Expr

polyline :: [Point] -> Path
polyline [] = EmptyPath
polyline (x:xs) = Path x (map StraightTo xs)

polygon :: [Point] -> Path
polygon [] = EmptyPath
polygon (x:xs) = Path x (map StraightTo xs ++ [Cycle])


-- | Circle approximated with 4 cubic bezier curves
circle :: Point -> Expr -> Path
circle center r =      Path (pt r 0)
                         [CurveTo (pt r k) (pt k r) (pt 0 r),
                          CurveTo (pt (-k) r) (pt (-r) k) (pt (-r) 0),
                          CurveTo (pt (-r) (-k)) (pt (-k) (-r)) (pt 0 (-r)),
                          CurveTo (pt k (-r)) (pt r (-k)) (pt r 0),
                          Cycle]
 where k1 :: Constant
       k1 = 4 * (sqrt 2 - 1) / 3
       k = k1 *^ r
       pt x y = center ^+^ (Point x y)


path :: Monad m => Path -> Diagram m ()
path p = do
  options <- view diaPathOptions
  tracePath' <- view (diaBackend . tracePath)
  freeze p (tracePath' options)

frozenPath' :: Monad m => FrozenPath -> Diagram m ()
frozenPath' p = do
  options <- view diaPathOptions
  tracePath' <- view (diaBackend . tracePath)
  freeze [] $ \_ -> tracePath' options p

stroke :: Monad m => Color -> Diagram m a -> Diagram m a
stroke color = using (outline color)

draw :: Monad m => Diagram m a -> Diagram m a
draw = stroke "black"

noOutline :: PathOptions -> PathOptions
noOutline = set drawColor Nothing

outline :: Color -> PathOptions -> PathOptions
outline color = set drawColor (Just color)

fill :: Color -> PathOptions -> PathOptions
fill color = set fillColor (Just color)

zigzagDecoration :: PathOptions -> PathOptions
zigzagDecoration = set decoration (Decoration "zigzag")

using :: Monad m => (PathOptions -> PathOptions) -> Diagram m a -> Diagram m a
using f = local (over diaPathOptions f)

ultraThin, veryThin, thin, semiThick, thick, veryThick, ultraThick :: Constant
ultraThin = 0.1
veryThin = 0.2
thin = 0.4
semiThick = 0.6
thick = 0.8
veryThick = 1.2
ultraThick = 1.6

solid, dotted, denselyDotted, looselyDotted, dashed, denselyDashed,
  looselyDashed, dashDotted, denselyDashdotted, looselyDashdotted :: PathOptions -> PathOptions
solid             o@PathOptions{..} = o { _dashPattern = [] }
dotted            o@PathOptions{..} = o { _dashPattern = [(_lineWidth,2)] }
denselyDotted     o@PathOptions{..} = o { _dashPattern = [(_lineWidth, 1)] }
looselyDotted     o@PathOptions{..} = o { _dashPattern = [(_lineWidth, 4)] }
dashed            o@PathOptions{..} = o { _dashPattern = [(3, 3)] }
denselyDashed     o@PathOptions{..} = o { _dashPattern = [(3, 2)] }
looselyDashed     o@PathOptions{..} = o { _dashPattern = [(3, 6)] }
dashDotted        o@PathOptions{..} = o { _dashPattern = [(3, 2), (_lineWidth, 2)] }
denselyDashdotted o@PathOptions{..} = o { _dashPattern = [(3, 1), (_lineWidth, 1)] }
looselyDashdotted o@PathOptions{..} = o { _dashPattern = [(3, 4), (_lineWidth, 4)] }

