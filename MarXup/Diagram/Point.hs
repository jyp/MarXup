{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell #-}

module MarXup.Diagram.Point where

import MarXup.Diagram.Layout
import Data.Traversable
import Data.Foldable
import Data.Algebra
import Control.Applicative
import Data.List (transpose)
import Prelude hiding (sum,mapM_,mapM,concatMap,maximum,minimum)

infix 4 .=.
----------------
-- Points 
-- | A point in 2d space
data Point' a = Point {xpart :: a, ypart :: a}
  deriving (Eq,Show)

instance Traversable Point' where
  traverse f (Point x y) = Point <$> f x <*> f y

instance Foldable Point' where
  foldMap = foldMapDefault

instance Functor Point' where
  fmap = fmapDefault

  
type Point = Point' Expr
instance Group a => Num (Point' a) where
  negate = neg
  (+) = (^+^)
  (-) = (^-^)

instance Group v => Group (Point' v) where
  zero = Point zero zero
  Point x1 y1 ^+^ Point x2 y2 = Point (x1 ^+^ x2) (y1 ^+^ y2)
  neg (Point x y) = Point (neg x) (neg y)

instance Module Constant v => Module Constant (Point' v) where
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
