{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell, RankNTypes #-}

module MarXup.Diagram.Tikz where

import Graphics.Diagrams.Core
import Graphics.Diagrams.Path
import Prelude hiding (sum,mapM_,mapM,concatMap)
import Data.List (intercalate)
import MarXup
import MarXup.MultiRef (newLabel)
import MarXup.Tex
import Numeric (showFFloat)
import Data.Foldable
import Data.Monoid

type TexDiagram = Diagram TeX Tex

instance Element (Diagram TeX Tex ()) where
  type Target (Diagram TeX Tex ()) = TeX
  element d = do
   texLn "" -- otherwise beamer does not understand where a tikzpicture ends (?!!)
   braces $ do
    usepkg "tikz" 100 []
    env "tikzpicture" $
      runDiagram tikzBackend d

-- diaDebug msg = diaRaw $ "\n%DBG:" ++ msg ++ "\n"
class Tikz a where
  toTikz :: a -> String

instance Tikz FrozenPoint where
  toTikz pt = frozenPointElim pt $ \x y -> "(" <> showDistance x <> "," <> showDistance y <> ")"

instance Tikz (Frozen Segment) where
  toTikz (StraightTo p) = "--" <> toTikz p
  toTikz (CurveTo c d p) = "..controls" <> toTikz c <> "and" <> toTikz d <> ".." <> toTikz p
  toTikz Cycle = "--cycle"
  -- toTikz (VH p) = "|-" <> toTikz p
  -- toTikz (HV p) = "-|" <> toTikz p
  -- toTikz (Rounded Nothing) = "[sharp corners]"
  -- toTikz (Rounded (Just r)) = "[" <> toTikz (constant r) <> "]"

showDistance :: Constant -> String
showDistance x = showFFloat (Just 4) x tikzUnit
    where tikzUnit = "pt"

instance Tikz LineTip where
  toTikz t = case t of
    ToTip -> "to"
    StealthTip -> "stealth"
    CircleTip -> "o"
    NoTip -> ""
    LatexTip -> "latex"
    ReversedTip x -> toTikz x ++ " reversed"
    BracketTip -> "["
    ParensTip -> "("

showDashPat :: DashPattern -> String
showDashPat xs = intercalate " " ["on " <> showDistance on <>
                                  " off " <> showDistance off | (on,off) <- xs]

instance Tikz PathOptions where
  toTikz PathOptions{..} = "["
    <> toTikz _startTip <> "-" <> toTikz _endTip <> ","
    <> col "draw" _drawColor
    <> col "fill" _fillColor
    <> "line width=" <> showDistance _lineWidth <> ","
    <> "line cap=" <> (case _lineCap of
                          RoundCap -> "round"
                          RectCap -> "rect"
                          ButtCap -> "butt") <> ","
    <> "line join=" <> (case _lineJoin of
                          RoundJoin -> "round"
                          BevelJoin -> "bevel"
                          MiterJoin -> "miter") <> ","
    <> "dash pattern=" <> showDashPat _dashPattern
    <> (case _decoration of
           Decoration [] -> ""
           Decoration d -> ",decorate,decoration=" ++ d)
    <> "]"
    where col attr = maybe "" (\c -> attr <> "=" <> c <> ",")

tikzBackend :: Backend TeX Tex
tikzBackend = Backend {..} where
  _tracePath options p = do
     tex $ "\\path"
       <> toTikz options
       <> case p of
         EmptyPath -> ""
         (Path start segs) -> toTikz start ++ concatMap toTikz segs
     tex ";\n"
  _traceLabel :: Monad x =>
                   (location -> (FrozenPoint -> Tex ()) -> x ()) -> -- freezer
                   (forall a. Tex a -> x a) -> -- embedder
                   location ->
                   Tex () -> -- label specification
                   x BoxSpec
  _traceLabel freezer embedder point lab = do
       bxId <- embedder $ Tex newLabel
       freezer point $ \p' -> do
         tex $ "\\node[anchor=north west,inner sep=0] at " ++ toTikz p'
         fillBox bxId True $ braces $ lab
         tex ";\n"
       embedder $ getBoxFromId bxId


type Dia = TexDiagram ()

