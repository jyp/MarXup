{-# LANGUAGE DataKinds, TypeFamilies, ConstraintKinds, OverloadedStrings, PolyKinds, EmptyDataDecls, MultiParamTypeClasses,FlexibleContexts, OverlappingInstances   #-}

module MarXup.Diagrams where

import MarXup.MetaPost
import MarXup.Tex
import MarXup.MultiRef (Label,Multi(Raw))
import Data.Monoid
import Control.Applicative
import GHC.Exts (Constraint)
import Data.List (intersperse)


allAnchors = Cons Center (Cons N (Cons NW(Cons W(Cons S
            (Cons SW(Cons SE(Cons E(Cons NE(Cons Baseline(Cons BaselineC(Cons BaselineE Nil)))))))))))


data Anchor = Center | N | NW | W | SW | S | SE | E | NE | Baseline | BaselineC | BaselineE
  deriving Show
           
data Anchor' :: Anchor -> * where
  Center' :: Anchor' Center
  Baseline' :: Anchor' Baseline
  NW' :: Anchor' NW
  N' :: Anchor' N
  NE' :: Anchor' NE
  E' :: Anchor' E 
  SE' :: Anchor' SE
  S' :: Anchor' S 
  SW' :: Anchor' SW
  W' :: Anchor' W
  BaselineE' :: Anchor' BaselineE
  BaselineC' :: Anchor' BaselineC
  
forget :: Anchor' a -> Anchor  
forget Center' = Center
forget Baseline' = Baseline
forget NW' = NW
forget N'  = N
forget NE' = NE
forget E'  = E 
forget SE' = SE
forget S'  = S 
forget SW' = SW
forget W'  = W
forget BaselineE' =  BaselineE
forget BaselineC' =  BaselineC
          
type D a = MP a
data ObjectRef anchors

corner :: Elem anchor (Cons NW (Cons NE (Cons SE (Cons SW Nil)))) => Anchor' anchor -> Expr Picture -> Expr Pair
corner NW' (Expr p) = Expr $ "ulcorner " <> p 
   
text :: TeX -> D (Expr (ObjectRef (Cons Center (Cons N (Cons NW(Cons W(Cons S(Cons SW(Cons SE(Cons E(Cons NE(Cons Baseline(Cons BaselineC(Cons BaselineE Nil))))))))))))))
text t = do
  l0 <- mpLabel
  let l = objectRef "p" l0
      p = Expr $ "q" <> show l0
  "picture " <> out l <> ";\n" 
  out p <> " = TEX(" <> mpQuote t <> ");\n"
  "pair " <> sequence_ (intersperse ", " $ [out (l <> "." <> Expr (show a)) | a <- toList allAnchors]) <> ";\n"
  
  (NW' ▸ l)  =-= (NE' ▸ l)
  SW' ▸ l  =-= SE' ▸ l
  NW' ▸ l  =|= SW' ▸ l
  NE' ▸ l  =|= NE' ▸ l
  center [NW' ▸ l, NE' ▸ l] === N' ▸ l
  center [SW' ▸ l, SE' ▸ l] === S' ▸ l
  center [SW' ▸ l, NW' ▸ l] === W' ▸ l
  center [SE' ▸ l, NE' ▸ l] === E' ▸ l
  center [S'  ▸ l, N' ▸ l] === Center' ▸ l
  
  NW' ▸ l === Baseline' ▸ l + (0 +: ypart (NW' `corner` p))
--  SW' ▸ l === Baseline' ▸ l - (RawExp $ "(0,ypart ulcorner " <> sho l <> ")")
  
  -- TODO: other constraints
  "draw " <> out l <> " shifted by " <> out (Baseline' ▸ l)
  return $ l
  
infix 8 ▸ 
(▸) :: Elem a anchors => Anchor' a -> Expr (ObjectRef anchors) -> Expr Pair
a ▸ (Expr x) = Expr $ x <> "." <> show (forget a)

objectRef :: String -> Label -> Expr (ObjectRef anchors)
objectRef prefix lab = Expr (prefix ++ show lab)

data List a = Nil | Cons a (List a)
  
toList Nil = []                    
toList (Cons x xs) =  x : (toList xs)

class Elem (a :: Anchor) (as :: List Anchor) where

instance Elem a (Cons a as) 
instance Elem a as => Elem a (Cons b as) 
  


