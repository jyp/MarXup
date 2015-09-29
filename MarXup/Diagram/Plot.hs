{-# LANGUAGE FlexibleContexts #-}
module MarXup.Diagram.Plot where

import MarXup.Diagram
import Control.Lens hiding ((#))
import MarXup
import MarXup.Tex
import Control.Monad (forM_,when)


axisGen :: Point -> Point -> Anchor -> [(Double,TeX)] -> Diagram ()
axisGen origin target anch labels = do
  draw $ using (set endTip ToTip) $ path $ polyline [origin,target]
  when (not $ null $ labels) $ do
    forM_ labels $ \(p,txt) -> do
      l <- labelObj txt
      l # anch .=. Point (lint p (xpart origin) (xpart target))
                         (lint p (ypart origin) (ypart target))

scale minx maxx x = (x - minx) / (maxx - minx)

mkSteps :: [Double] -> [(Double,TeX)]
mkSteps xs = [(scale minx maxx x, textual $ show x) | x <- xs]
  where maxx = maximum xs
        minx = minimum xs

hAxis :: Box -> [(Double, TeX)] -> Diagram ()
hAxis bx = axisGen (bx # SW) (bx # SE) N
vAxis :: Box -> [(Double, TeX)] -> Diagram ()
vAxis bx = axisGen (bx # SW) (bx # NW) E

lint :: Constant -> Expr -> Expr -> Expr
lint p origin target = (p*-(target-origin)) + origin

scatterPlot :: Box -> [(Double,Double)] -> Diagram ()
scatterPlot bx input = forM_ input $ \(x,y) -> do
  pt <- using (fill "black") $ circleShape
  let lx = xpart (bx # SW)
      ly = ypart (bx # SW)
      hx = xpart (bx # NE)
      hy = ypart (bx # NE)
  width pt === constant 3
  pt # Center .=. Point (lint x lx hx) (lint y ly hy)


simplePlot :: [Double] -> [Double] -> [(Double,Double)] -> Diagram Box
simplePlot _ _ [] = do
  labelObj $ textual "NO DATA"
simplePlot xs0 ys0 input = do
  bx <- rectangleShape =<< box
  
  let maxx = maximum xs
      minx = minimum xs
      maxy = maximum ys
      miny = minimum ys
      normalize (x,y) = (scale minx maxx x, scale miny maxy y)
      xs = if length xs0 < 2 then bnds (map fst input) else xs0
      ys = if length ys0 < 2 then bnds (map snd input) else ys0
      bnds zs = [minimum zs, maximum zs]
  hAxis bx $ mkSteps xs
  vAxis bx $ mkSteps ys
  scatterPlot bx $ map normalize input
  return bx

