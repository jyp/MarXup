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
   
abstractBox :: D (Expr ObjectRef)
abstractBox = do
  l <- mkRef "p" <$> mpLabel
  "pair " <> sequence_ (intersperse ", " $ [out (l <> "." <> Expr (show a)) | a <- toList allAnchors]) <> ";\n"
  center [NW ▸ l, NE ▸ l] === N ▸ l
  center [SW ▸ l, SE ▸ l] === S ▸ l
  center [SW ▸ l, NW ▸ l] === W ▸ l
  center [SE ▸ l, NE ▸ l] === E ▸ l
  center [SW  ▸ l, NE ▸ l] === Center ▸ l
  NW ▸ l  =-= NE ▸ l
  SW ▸ l  =-= SE ▸ l
  NE ▸ l  =|= SE ▸ l
  NW ▸ l  =|= SW ▸ l
  E ▸ l  =|= BaselineE ▸ l
  W ▸ l =|= Baseline ▸ l
  center [Baseline ▸ l, BaselineE ▸ l] === BaselineC ▸ l
  return l

height o = ypart (N ▸ o - S ▸ o)
width o = xpart (E ▸ o - W ▸ o)

drawBounds :: Expr ObjectRef -> D ()
drawBounds l = delay $ do
     defaultVal (width l) 20
     defaultVal (height l) 10
     defaultVal (ypart (Center ▸ l)) (ypart (Baseline ▸ l))
     defaultVal (xpart (Baseline ▸ l)) 0
     defaultVal (ypart (Baseline ▸ l)) 0
     "draw " <> out (NW ▸ l) <> "--" <> out (NE ▸ l) <> "--" <> out (SE ▸ l) <> "--" <> out (SW ▸ l) <> "-- cycle;\n"
  
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
     -- "draw " <> out (NW ▸ l) <> "--" <> out (NE ▸ l) <> "--" <> out (SE ▸ l) <> "--" <> out (SW ▸ l) <> "-- cycle withcolor red;\n"
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

