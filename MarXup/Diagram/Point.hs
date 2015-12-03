{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell #-}

module MarXup.Diagram.Point where

import MarXup.Diagram.Layout
import Data.Foldable
import Control.Applicative
import Data.List (transpose)
import Prelude hiding (sum,mapM_,mapM,concatMap,maximum,minimum)

infix 4 .=.
----------------
-- Points 
-- | A point in 2d space


type Point = Point' Expr

-- | Orthogonal norm of a vector
orthonorm :: Monad m => Point -> Diagram m Expr
orthonorm (Point x y) =
  (+) <$> absoluteValue x <*> absoluteValue y

-- | Orthogonal distance between points.
orthoDist :: Monad m => Point -> Point -> Diagram m Expr
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

(.=.),northOf,southOf,westOf,eastOf :: Monad m => Point -> Point -> Diagram m ()
Point x1 y1 .=. Point x2 y2 = do
  x1 === x2
  y1 === y2

northOf (Point _ y1) (Point _ y2) = y2 <== y1
southOf = flip northOf
westOf (Point x1 _) (Point x2 _) = x1 <== x2
eastOf = flip westOf

alignHoriz,alignVert :: Monad m => [Point] -> Diagram m ()
alignHoriz = align ypart
alignVert = align xpart

align :: Monad m => (a -> Expr) -> [a] -> Diagram m ()
align _ [] = return ()
align f (p:ps) = forM_ ps $ \p' -> f p === f p'

alignMatrix :: Monad m => [[Point]] -> Diagram m ()
alignMatrix ls = do
  forM_ ls alignHoriz
  forM_ (transpose ls) alignVert

---------------------
-- Point objectives

southwards, northwards, westwards, eastwards :: Monad m => Point -> Diagram m ()
southwards (Point _ y) = minimize y
westwards (Point x _) = minimize x
northwards = southwards . negate
eastwards = westwards . negate
