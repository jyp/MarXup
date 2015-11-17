{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module MarXup.Diagram.Plot where

import MarXup.Diagram
import Control.Lens hiding ((#))
import MarXup
import MarXup.Tex
import Control.Monad (forM_,when)

type Transform = Double -> Double

type Vec a = (a,a) -- TODO: make this a proper applicative to simplify the code.

-- | Generic axis rendering. @axisGen origin target anchor labels@
-- traces an axis from origin to target, attaching the labels at
-- anchor.
axisGen :: Point -> Point -> Anchor -> [(Double,TeX)] -> Diagram ()
axisGen origin target anch labels = do
  draw $ using (set endTip ToTip) $ path $ polyline [origin,target]
  when (not $ null $ labels) $ do
    forM_ labels $ \(p,txt) -> do
      l <- labelObj txt
      l # anch .=. Point (lint p (xpart origin) (xpart target))
                         (lint p (ypart origin) (ypart target))

-- | @scale minx maxx@ maps the interval [minx,maxx] to [0,1]
scale :: forall a. Fractional a => a -> a -> a -> a
scale minx maxx x = (x - minx) / (maxx - minx)

-- | Make a number of steps
mkSteps :: Transform -> (Double -> String) -> [Double] -> [(Double,TeX)]
mkSteps tx showFct xs = [(x, textual $ showFct x) | x0 <- xs, let x = tx x0]

-- | render an horizontal axis on the given box
hAxis :: Box -> [(Double, TeX)] -> Diagram ()
hAxis bx = axisGen (bx # SW) (bx # SE) N

-- | render a vertical axis on the given box
vAxis :: Box -> [(Double, TeX)] -> Diagram ()
vAxis bx = axisGen (bx # SW) (bx # NW) E

-- | Multiply the vector (origin --> target) by p.
lint :: Constant -> Expr -> Expr -> Expr
lint p origin target = (p*-(target-origin)) + origin

-- | Draw a scatterplot in the given box.
-- Input data in the [0,1] interval fits the box.
scatterPlot :: Box -> [(Double,Double)] -> Diagram ()
scatterPlot bx input = forM_ input $ \(x,y) -> do
  pt <- using (fill "black") $ circleShape
  let lx = xpart (bx # SW)
      ly = ypart (bx # SW)
      hx = xpart (bx # NE)
      hy = ypart (bx # NE)
  width pt === constant 3
  pt # Center .=. Point (lint x lx hx) (lint y ly hy)

-- | @xformPlot transforms bounds marks inputdata@. Draw a 2d scatterplot. @marks@ is a
-- list of points to mark on the axes.
xformPlot :: Vec Transform -> Vec [Double] -> [Vec Double] -> Diagram Box
xformPlot _ _ [] = do
  labelObj $ textual "NO DATA"
xformPlot (tx,ty) (xs,ys) input = do
  bx <- rectangleShape =<< box
  let normalize (x,y) = (tx x, ty y)
  hAxis bx $ mkSteps tx show xs
  vAxis bx $ mkSteps ty show ys
  scatterPlot bx $ map normalize input
  return bx

type AxisGen = (Double -> Double -> (Double, [Double], Double), Transform)

-- TODO: compute lower bound intelligently
logAxis :: Double -> AxisGen
logAxis base = (\_lo hi -> (1,takeWhile (< hi) (map (base ^^) [(0::Integer)..]),hi)
               ,\x -> log x / log base)

simplLinAxis :: Double -> AxisGen
simplLinAxis step = (\_lo hi -> (0,takeWhile (< hi) $ map (*step) [0,1],hi),
                     id)

-- | Draw a 2D scatter plot, given an axis specification and a data
-- set
plot :: Vec AxisGen -> [Vec Double] -> Diagram Box
plot ((gx,tx),(gy,ty)) input = xformPlot (scale minx maxx . tx, scale miny maxy . ty) (xs,ys) input
  where (minx,xs,maxx) = uncurry gx (bnds (map fst input))
        (miny,ys,maxy) = uncurry gy (bnds (map snd input))
        bnds zs = (minimum zs, maximum zs)

