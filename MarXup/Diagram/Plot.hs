{-# LANGUAGE FlexibleContexts, RankNTypes, DeriveFunctor #-}
module MarXup.Diagram.Plot where

import MarXup.Diagram
import Control.Lens hiding ((#))
import MarXup
import MarXup.Tex
import Control.Monad (forM_,when)

type Transform = Double -> Double

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
  draw $ using (set endTip ToTip) $ path $ polyline [origin,target]
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
mkSteps :: Transform -> ShowFct -> [Double] -> [(Double,TeX)]
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


type AxisGen = (Double -> Double -> (Double, [Double], Double), Transform)

-- TODO: compute lower bound intelligently
logAxis :: Double -> AxisGen
logAxis base = (\_lo hi -> (1,takeWhile (< hi) (map (base ^^) [(0::Integer)..]),hi)
               ,\x -> log x / log base)

simplLinAxis :: Double -> AxisGen
simplLinAxis step = (\_lo hi -> (0,takeWhile (< hi) $ map (*step) [0..],hi),
                     id)

frst (x,_,_) = x
scnd (_,x,_) = x
thrd (_,_,x) = x

type ShowFct = Double -> ShowS

mkAxes :: Vec2 ShowFct -> Vec2 AxisGen -> Vec2 Double -> Vec2 Double -> (Vec2 [(Double,TeX)], Vec2 Transform)
mkAxes showFct axGen lo hi = (marks, xform)
  where axisInfo = fst <$> axGen <*> lo <*> hi
        zs = scnd <$> axisInfo
        minz = frst <$> axisInfo
        maxz = thrd <$> axisInfo
        xform = (.) <$> scales <*> (snd <$> axGen)
        marks = (mkSteps <$> xform <*> showFct <*> zs)
        scales = scale <$> minz <*> maxz

-- | Draw a 2D scatter plot, given an axis specification and a data
-- set
simplePlot :: Vec2 ShowFct -> Vec2 AxisGen -> [Vec2 Double] -> Diagram Box
simplePlot showFct axGen input = do
  bx <- rectangleShape =<< box
  axes bx marks
  scatterPlot bx (map (xform <*>) input)
  return bx
  where input' = sequenceA input
        (marks,xform) = mkAxes showFct axGen (minimum <$> input') (maximum <$> input')
