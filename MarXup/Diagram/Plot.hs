{-# LANGUAGE FlexibleContexts, RankNTypes, DeriveFunctor #-}
module MarXup.Diagram.Plot where

import MarXup.Diagram
-- import Control.Lens hiding (set)
import MarXup
import MarXup.Tex
import Control.Monad (forM_,when)

type Transform a = Equiv a Constant

data Vec2 a = Vec2 a a
  deriving (Functor)

instance Applicative Vec2 where
  pure x = Vec2 x x
  Vec2 f g <*> Vec2 x y = Vec2 (f x) (g y)

-- | Generic axis rendering. @axisGen origin target anchor labels@
-- traces an axis from origin to target, attaching the labels at
-- anchor.
axisGen :: Point -> Point -> Anchor -> [(Constant,TeX)] -> Diagram ()
axisGen origin target anch labels = do
  draw {- using (set endTip ToTip) -} $ path $ polyline [origin,target]
  when (not $ null $ labels) $ do
    forM_ labels $ \(p,txt) -> do
      l0 <- labelObj txt
      let l = extend 3 (anchors l0)
      draw $ path $ polyline [l0 # anch, l # anch]
      l # anch .=. Point (lint p (xpart origin) (xpart target))
                         (lint p (ypart origin) (ypart target))

-- | @scale minx maxx@ maps the interval [minx,maxx] to [0,1]
scale :: forall b. Fractional b => b -> b -> Equiv b b
scale minx maxx = Equiv (\x -> (x - minx) / (maxx - minx))
                        (\x -> x * (maxx - maxx) + minx)

-- | Make a number of steps
mkSteps :: Transform a -> ShowFct a -> [a] -> [(Constant,TeX)]
mkSteps tx showFct xs = zip (map (forward tx) xs) (map (textual . ($ []) .  showFct) xs)

-- | render an horizontal axis on the given box
hAxis :: Box -> [(Constant, TeX)] -> Diagram ()
hAxis bx = axisGen (bx # SW) (bx # SE) N

-- | render a vertical axis on the given box
vAxis :: Box -> [(Constant, TeX)] -> Diagram ()
vAxis bx = axisGen (bx # SW) (bx # NW) E

-- | Draw axes. Coordinates in the [0,1] fit the box.
axes :: Box -> Vec2 [(Constant, TeX)] -> Diagram ()
axes bx zs = d1 >> d2
  where Vec2 d1 d2 = (Vec2 hAxis vAxis) <*> pure bx <*> zs

-- | Multiply the vector (origin --> target) by p.
lint :: Constant -> Expr -> Expr -> Expr
lint p origin target = (p*-(target-origin)) + origin

-- | Draw a scatterplot in the given box.
-- Input data in the [0,1] interval fits the box.
scatterPlot :: Box -> [Vec2 Constant] -> Diagram ()
scatterPlot bx input = forM_ input $ \z -> do
  pt <- using (fill "black") $ circleShape
  width pt === constant 3
  pt # Center .=. interpBox bx (v2p z)

v2p :: forall a. Vec2 a -> Point' a
v2p (Vec2 x y) = Point x y


interpBox :: forall a. Anchored a => a -> Point' Constant -> Point' Expr
interpBox bx z = lint <$> z <*> bx#SW <*> bx#NE

-- | @functionPlot bx n xform f@.
-- Plot the function @f@ on the box @bx@,
-- using @n@ steps (precision). @xform@ is the axis transformation given by @mkAxes@.

functionPlot :: Box -> Int -> Transform a -> (a -> a) -> Diagram ()
functionPlot bx nsteps t f = draw $ path $ polyline points
  where points = do
           xi <- (map ( (/fromIntegral nsteps) . fromIntegral) [0..nsteps])
           let x = backward t xi
               y = f x
               yi = forward t y
           return $ interpBox bx (Point xi yi)

data Equiv a b = Equiv {forward :: a -> b, backward :: b -> a}

after :: Equiv b c -> Equiv a b -> Equiv a c
(Equiv f g) `after` (Equiv h i) = Equiv (f . h) (i . g)

axisMarks :: a -> a -> Equiv a Constant -> (a,[a],a)
axisMarks lo hi trans = (u lo',(map u [lo'..hi']),u hi')
  where u = backward trans
        t = forward trans
        lo' = fromIntegral $ floor (t lo)
        hi' = fromIntegral $ ceiling (t hi)

logAxis :: Constant -> Transform Constant
logAxis base = Equiv t u
     where t x = log x / log base
           u x = base ** x

simplLinAxis :: Constant -> Transform Constant
simplLinAxis step = Equiv (/step) (*step)

type ShowFct a = a -> ShowS

mkAxes :: Vec2 (Transform a) -> Vec2 a -> Vec2 a -> (Vec2 [a], Vec2 (Transform a))
mkAxes axesXform lows highs = (mrks <$> axisInfo,
                               after <$> (scale <$> minz <*> maxz) <*> axesXform)
  where axisInfo = axisMarks <$> lows <*> highs <*> axesXform
        minz = t <*> (lo <$> axisInfo)
        maxz = t <*> (hi <$> axisInfo)
        t = forward <$> axesXform
        lo (x,_,_) = x
        mrks (_,x,_) = x
        hi (_,_,x) = x

type PlotCanvas a = (Box, Vec2 (Transform a))

preparePlot :: Vec2 (ShowFct a) -> Vec2 (Transform a) -> Vec2 a -> Vec2 a -> Diagram (PlotCanvas a)
preparePlot showFct axesXform lo hi = do
  bx <- rectangleShape =<< box
  axes bx marks
  return (bx,xform)
  where marks = mkSteps <$> xform <*> showFct <*> marks0
        (marks0,xform) = mkAxes axesXform lo hi

-- | Draw a 2D scatter plot, given an axis specification and a data
-- set
simplePlot :: Ord a => Vec2 (ShowFct a) -> Vec2 (Transform a) -> [Vec2 a] -> Diagram Box
simplePlot showFct axesXform input = do
  (bx,xform) <- preparePlot showFct axesXform (minimum <$> input') (maximum <$> input')
  scatterPlot bx (map (forward <$> xform <*>) input)
  return bx
  where input' = sequenceA input
