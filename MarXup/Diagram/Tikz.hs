{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, OverloadedStrings, RecordWildCards, UndecidableInstances, RankNTypes #-}

module MarXup.Diagram.Tikz (tikzPicture, tikzZeroBaseline, tikzCentered, tikzLarge, TexDiagram, showDistance) where

import Graphics.Diagrams.Core
import Prelude hiding (sum,mapM_,mapM,concatMap)
import Data.List (intercalate)
import MarXup
import MarXup.MultiRef (newLabel)
import MarXup.Tex
import Numeric (showFFloat)
import Data.Foldable

type TexDiagram = Diagram TeX Tex

-- | Tikz picture with options
tikzPicture, tikzZeroBaseline, tikzCentered, tikzLarge :: [String] -> Diagram TeX Tex b -> Tex b
tikzPicture options d = do
  -- texLn "" -- otherwise beamer does not understand where a tikzpicture ends (?!!) -- This is a bad hack because it inserts paragaph breaks
  braces $ do
    usepkg "tikz" 100 []
    -- useTikzLib "decorations.pathmorphing" -- yields "dimension too large in some paths"
    env'' "tikzpicture" (tex <$> options) [] $ -- no escaping of options, because braces are common here
      runDiagram tikzBackend d


-- | Convert a diagram to tikz, and let the baseline be at 0 (instead
-- of the lowest point in the picture). This way the baseline can be
-- set precisely in the diagram (knowing that 0 y coordinate is
-- baseline).
tikzZeroBaseline options = tikzPicture ("baseline=0pt":options)

-- | Convert a diagram to tikz, and let the baseline be 0.8x below the
-- center of the picture. This is suitable for larger pictures
-- inserted directly in text.
tikzLarge options = tikzPicture ("baseline={([yshift=-0.8ex]current bounding box.center)}":options)

-- | Convert a diagram to tikz, and let the baseline be the center of the picture.
-- This is suitable for lining up pictures by center (for example in a table)
tikzCentered options = tikzPicture ("baseline=(current bounding box.center)":options)


instance Element (Diagram TeX Tex ()) where
  type Target (Diagram TeX Tex ()) = TeX
  element = tikzPicture []

-- diaDebug msg = diaRaw $ "\n%DBG:" ++ msg ++ "\n"
class Tikz a where
  toTikz :: a -> String

instance Tikz FrozenPoint where
  toTikz (Point x y) =  "(" <> showDistance x <> "," <> showDistance y <> ")"

instance Tikz (Frozen Segment) where
  toTikz (StraightTo p) = "--" <> toTikz p
  toTikz (CurveTo c d p) = "..controls" <> toTikz c <> "and" <> toTikz d <> ".." <> toTikz p
  toTikz Cycle = "--cycle"
  -- toTikz (VH p) = "|-" <> toTikz p
  -- toTikz (HV p) = "-|" <> toTikz p
  -- toTikz (Rounded Nothing) = "[sharp corners]"
  -- toTikz (Rounded (Just r)) = "[" <> toTikz (constant r) <> "]"

showDistance :: Constant -> String
showDistance x = dropZeros (showFFloat (Just 4) x "") <> tikzUnit
    where tikzUnit = "pt"
          dropZeros = reverse . dropDot . dropWhile (== '0') . reverse
          dropDot ('.':xs) = xs
          dropDot xs = xs

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
           Decoration _ -> error "decorations not supported! (MarXup.Diagram.Tikz)"
             -- ",decorate,decoration=" ++ d
       )
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
