{-# LANGUAGE FlexibleContexts, RankNTypes, DeriveFunctor #-}
module MarXup.Diagram.Plot where

import MarXup.Diagram
import Control.Lens hiding ((#))
import MarXup
import MarXup.Tex
import Control.Monad (forM_,when)

type Transform a = a -> Double

data Vec2 a = Vec2 a a
  deriving (Functor)

instance Applicative Vec2 where
  pure x = Vec2 x x
  Vec2 f g <*> Vec2 x y = Vec2 (f x) (g y)

-- | Generic axis rendering. @axisGen origin target anchor labels@
-- traces an axis from origin to target, attaching the labels at
-- anchor.
axisGen :: Point -> Point -> Anchor -> [(Double,TeX)] -> Diagram ()
axisGen origin target anch labels = do
  draw {-$ using (set endTip ToTip)-} $ path $ polyline [origin,target]
  when (not $ null $ labels) $ do
    forM_ labels $ \(p,txt) -> do
      l0 <- labelObj txt
      let l = extend 3 (anchors l0)
      draw $ path $ polyline [l0 # anch, l # anch]
      l # anch .=. Point (lint p (xpart origin) (xpart target))
                         (lint p (ypart origin) (ypart target))

-- | @scale minx maxx@ maps the interval [minx,maxx] to [0,1]
scale :: forall a. Fractional a => a -> a -> a -> a
scale minx maxx x = (x - minx) / (maxx - minx)

-- | Make a number of steps
mkSteps :: Transform a -> ShowFct a -> [a] -> [(Double,TeX)]
mkSteps tx showFct xs = zip (map tx xs) (map (textual . ($ []) .  showFct) xs)

-- | render an horizontal axis on the given box
hAxis :: Box -> [(Double, TeX)] -> Diagram ()
hAxis bx = axisGen (bx # SW) (bx # SE) N

-- | render a vertical axis on the given box
vAxis :: Box -> [(Double, TeX)] -> Diagram ()
vAxis bx = axisGen (bx # SW) (bx # NW) E

-- | Draw axes. Coordinates in the [0,1] fit the box.
axes :: Box -> Vec2 [(Double, TeX)] -> Diagram ()
axes bx zs = d1 >> d2
  where Vec2 d1 d2 = (Vec2 hAxis vAxis) <*> pure bx <*> zs

-- | Multiply the vector (origin --> target) by p.
lint :: Constant -> Expr -> Expr -> Expr
lint p origin target = (p*-(target-origin)) + origin

-- | Draw a scatterplot in the given box.
-- Input data in the [0,1] interval fits the box.
scatterPlot :: Box -> [Vec2 Double] -> Diagram ()
scatterPlot bx input = forM_ input $ \(Vec2 x y) -> do
  pt <- using (fill "black") $ circleShape
  let lx = xpart (bx # SW)
      ly = ypart (bx # SW)
      hx = xpart (bx # NE)
      hy = ypart (bx # NE)
  width pt === constant 3
  pt # Center .=. Point (lint x lx hx) (lint y ly hy)


-- | An axis generator is a pair of
-- 1. a function taking a lo and a hi bound and returning a lo, hi,
-- and a list of intermediate steps to mark on the graph.  2. a
-- transformation function mapping raw coordinates into coordinates of
-- the axis.
type AxisGen a = (a -> a -> (a, [a], a), Transform a)

-- TODO: compute lower bound intelligently
logAxis :: Double -> AxisGen Double
logAxis base = (\lo hi -> let lo' :: Int
                              lo' = floor (t lo)
                              hi' = ceiling (t hi)
                          in (u lo',(map u [lo'..hi']),u hi')
               ,t)
               where t x = log x / log base
                     u x = base ^^ x

simplLinAxis :: Double -> AxisGen Double
simplLinAxis step = (\_lo hi -> let hi' :: Int
                                    hi' = ceiling (t hi)
                                in (0,map u [0..hi'],u hi'),
                     id)
               where t x = x / step
                     u x = step * fromIntegral x


type ShowFct a = a -> ShowS

mkAxes :: Vec2 (ShowFct a) -> Vec2 (AxisGen a) -> Vec2 a -> Vec2 a -> (Vec2 [(Double,TeX)], Vec2 (Transform a))
mkAxes showFct axGen lows highs = (marks, xform)
  where axisInfo = fst <$> axGen <*> lows <*> highs
        zs = mrks <$> axisInfo
        minz = t <*> (lo <$> axisInfo)
        maxz = t <*> (hi <$> axisInfo)
        xform = (.) <$> scales <*> t
        marks = mkSteps <$> xform <*> showFct <*> zs
        scales = scale <$> minz <*> maxz
        t = snd <$> axGen
        lo (x,_,_) = x
        mrks (_,x,_) = x
        hi (_,_,x) = x

-- | Draw a 2D scatter plot, given an axis specification and a data
-- set
simplePlot :: Ord a => Vec2 (ShowFct a) -> Vec2 (AxisGen a) -> [Vec2 a] -> Diagram Box
simplePlot showFct axGen input = do
  bx <- rectangleShape =<< box
  axes bx marks
  scatterPlot bx (map (xform <*>) input)
  return bx
  where input' = sequenceA input
        (marks,xform) = mkAxes showFct axGen (minimum <$> input') (maximum <$> input')
