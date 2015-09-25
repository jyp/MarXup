{-# LANGUAGE FlexibleContexts #-}
module MarXup.Diagram.Plot where

import MarXup.Diagram
import Control.Lens hiding ((#))
import MarXup
import MarXup.Tex
import Control.Monad (forM_,when)


axisGen :: Point -> Point -> Anchor -> [TeX] -> Diagram ()
axisGen origin target anch labels = do
  using (set endTip ToTip) $ path $ polyline [origin,target]
  when (not $ null $ labels) $ do
    forM_ [0..length labels] $ \i -> do
      l <- labelObj $ labels !! i
      l # anch .=. ((fromIntegral i / fromIntegral (length labels - 1)) *- (target - origin)) + origin

axisSteps :: Point -> Point -> Anchor -> Double -> Double -> Int -> Diagram ()
axisSteps origin target anch lo hi npts =
  axisGen origin target anch [textual $ show (lo+(hi-lo)*fromIntegral i/fromIntegral npts) | i <- [0..npts] ]

hAxis :: Box -> Double -> Double -> Int -> Diagram ()
hAxis bx lo hi npts = axisSteps (bx # SW) (bx # SE) N lo hi npts

vAxis :: Box -> Double -> Double -> Int -> Diagram ()
vAxis bx lo hi npts = axisSteps (bx # SW) (bx # NW) E lo hi npts

lint :: Constant -> Expr -> Expr -> Expr
lint p origin target = (p*-(target-origin)) + origin

plot :: Box -> [(Double,Double)] -> Diagram ()
plot bx input = forM_ input $ \(x,y) -> do
  pt <- circleShape
  let lx = xpart (bx # SW)
      ly = ypart (bx # SW)
      hx = xpart (bx # NE)
      hy = ypart (bx # NE)
  pt # Center .=. Point (lint x lx hx) (lint y ly hy)


simplePlot :: [(Double,Double)] -> Diagram ()
simplePlot [] = do
  labelObj $ textual "NO DATA"
  return ()
simplePlot input = do
  bx <- rectangleShape =<< box
  draw $ do
    hAxis bx 0 (maximum $ map fst $ input) 2
    vAxis bx 0 (maximum $ map snd $ input) 2
    plot bx input
