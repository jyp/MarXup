{-# LANGUAGE DataKinds, KindSignatures, OverloadedStrings, EmptyDataDecls, MultiParamTypeClasses, FlexibleContexts, OverlappingInstances   #-}

module MarXup.Diagrams where

import MarXup.MetaPost
import MarXup.Tex
import MarXup.MultiRef (Label,Multi(Raw))
import Data.Monoid
import Control.Applicative
import Data.List (intersperse)
import Data.Char (ord,chr)
import Numeric (showIntAtBase)

data Anchor = Center | N | NW | W | SW | S | SE | E | NE | Baseline | BaselineC | BaselineE
  deriving Show

allAnchors = Cons Center (Cons N (Cons NW(Cons W(Cons S
            (Cons SW(Cons SE(Cons E(Cons NE(Cons Baseline(Cons BaselineC(Cons BaselineE Nil)))))))))))

type D a = MP a
data ObjectRef
data Equation

corner :: Anchor -> Expr Picture -> Expr Pair
corner NW (Expr p) = Expr $ "ulcorner " <> p 
corner SW (Expr p) = Expr $ "llcorner " <> p 
corner NE (Expr p) = Expr $ "urcorner " <> p 
corner SE (Expr p) = Expr $ "lrcorner " <> p 

hdist,vdist :: Expr ObjectRef -> Expr ObjectRef -> Expr Numeric
hdist x y = xpart (W ▸ y - E ▸ x)
vdist x y = ypart (S ▸ y - N ▸ x)

boundingZone :: Expr ObjectRef -> Expr Path
boundingZone l = (NW ▸ l ... NE ▸ l ... SE ▸ l ... SW ▸ l ... closed) 

arrow :: [Expr DrawOption] -> Expr ObjectRef -> Expr ObjectRef -> MP (Expr Pair)
arrow opts source target = do
  c <- mkRef "z" <$> mpLabel
  mpRaw "pair " <> out c <>";\n"
  delay $ drawArrow (Center ▸ source ... open (Center ▸ target))
    (opts ++ [cutAfter $ boundingZone target,
              cutBefore $ boundingZone source])
  c === center [Center ▸ source,Center ▸ target]
  return c

shiftInDir :: Anchor -> Expr Numeric -> Expr Pair
shiftInDir N d = 0 +: d
shiftInDir S d = 0 +: negate d
shiftInDir W d = negate d +: 0
shiftInDir E d = d +: 0
shiftInDir NW d = negate d +: d
shiftInDir SE d = d +: negate d
shiftInDir SW d = negate d +: negate d
shiftInDir NE d = d +: d
shiftInDir _ _ = 0 +: 0

labelPt :: TeX -> Anchor -> Expr Pair -> MP (Expr ObjectRef)
labelPt labell anchor labeled  = do
  t <- textObj labell
  shiftInDir anchor 2 + (anchor ▸ t) === labeled
  return t

abstractBox :: D (Expr ObjectRef)
abstractBox = do
  l <- mkRef "p" <$> mpLabel
  "pair " <> sequence_ (intersperse ", " $ [out (l <> "." <> Expr (show a)) | a <- toList allAnchors]) <> ";\n"
  alignMatrix $ (map (map (▸ l))) 
      [[NW, N, NE]
      ,[W, Center, E]
      ,[Baseline, BaselineC, BaselineE]
      ,[SW, S, SE]
      ]
  ypart (N ▸ l - Center ▸ l) === ypart (Center ▸ l - S ▸ l)
  xpart (W ▸ l - Center ▸ l) === xpart (Center ▸ l - E ▸ l)
  
  return l

height o = ypart (N ▸ o - S ▸ o)
width o = xpart (E ▸ o - W ▸ o)

freezeBounds ::  Expr ObjectRef -> D ()
freezeBounds l = do
     defaultVal (width l) 20
     defaultVal (height l) 10
     defaultVal (ypart (Center ▸ l)) (ypart (Baseline ▸ l))
     defaultVal (xpart (Baseline ▸ l)) 0
     defaultVal (ypart (Baseline ▸ l)) 0


drawBounds :: Expr ObjectRef -> D ()
drawBounds l = delay $ do
  freezeBounds l 
  draw (NW ▸ l .-- NE ▸ l .-- SE ▸ l .-- SW ▸ l .-- closed) []

boxObj :: D (Expr ObjectRef)
boxObj = do
  l <- abstractBox
  drawBounds l
  return l

textObj :: TeX -> D (Expr ObjectRef)
textObj t = do
  p <- mkRef "q" <$> mpLabel
  "picture " <> out p <> ";\n" 
  out p <> " := " <> mpTex t <> ";\n"
  l <- abstractBox
  
  ypart (NW ▸ l - Baseline ▸ l - NW `corner` p) === 0
  ypart (SW ▸ l - Baseline ▸ l - SW `corner` p) === 0
  xpart (BaselineE ▸ l - Baseline ▸ l - NE `corner` p) === 0

  delay $ do
     defaultVal (xpart (Baseline ▸ l)) 0
     defaultVal (ypart (Baseline ▸ l)) 0
     "draw " <> out p <> " shifted " <> out (Baseline ▸ l) <> ";\n"
     -- "draw " <> out (Center ▸ l) <> ";\n"
  return l

infix 8 ▸ 
(▸) :: Anchor -> Expr ObjectRef -> Expr Pair
a ▸ (Expr x) = Expr $ x <> "." <> show a

mkRef :: String -> Label -> Expr a
mkRef prefix lab = Expr (prefix ++ encode lab)

encode :: Label -> String
encode n = showIntAtBase 16 (\x -> chr (ord 'a' + x)) n []

data List a = Nil | Cons a (List a)
  
toList Nil = []
toList (Cons x xs) =  x : (toList xs)

class Elem (a :: Anchor) (as :: List Anchor) where

instance Elem a (Cons a as) 
instance Elem a as => Elem a (Cons b as) 

