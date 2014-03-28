{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecursiveDo, TypeFamilies, OverloadedStrings, RecordWildCards,UndecidableInstances, PackageImports, TemplateHaskell #-}

module MarXup.Diagram.Tikz where

import MarXup.Diagram.Layout
import MarXup.Diagram.Point
import MarXup.Diagram.Path
import Control.Lens hiding (element)
import Prelude hiding (sum,mapM_,mapM,concatMap)
import Control.Applicative
import Data.List (intercalate)
import Data.String
import MarXup
import MarXup.MultiRef
import MarXup.Tex
import Numeric (showFFloat)
import Data.Traversable
import Data.Foldable
import Data.Monoid
import Control.Monad.Reader

instance Element Expr where
  type Target Expr = Dia
  element x = do
    v <- valueOf x
    diaRaw $ showDistance v

instance Element (Diagram ()) where
  type Target (Diagram ()) = TeX
  element d = do
   texLn "" -- otherwise beamer does not understand where a tikzpicture ends (?!!)
   braces $ do
    cmd0 "normalsize"
      -- otherwise the boxes use "normalsize", while tikz inherits
      -- the smaller or bigger size from the current scope. Actually,
      -- every text styling should be reset, but I don't know how to
      -- do that.
    env "tikzpicture" $
      Tex $ runDiagram d

--------------------
-- Point rendering
instance Element Point where
  type Target Point = Diagram ()
  element (Point x y) = "(" <> element x <> "," <> element y <> ")"



diaDebug msg = diaRaw $ "\n%DBG:" ++ msg ++ "\n"

instance (Element point,Monoid (Target point), IsString (Target point)) => Element (Segment point) where
  type Target (Segment point) = Target point
  element (StraightTo p) = "--" <> element p
  element (CurveTo c d p) = "..controls" <> element c <> "and" <> element d <> ".." <> element p
  element Cycle = "--cycle"
  -- element (VH p) = "|-" <> element p
  -- element (HV p) = "-|" <> element p
  -- element (Rounded Nothing) = "[sharp corners]"
  -- element (Rounded (Just r)) = "[" <> element (constant r) <> "]"

instance Element Path where
  type Target Path = Diagram ()
  element = path

path :: Path -> Dia
path = frozenPath <=< freezePath

frozenPath :: FrozenPath  -> Dia
frozenPath p  = do
  options <- view diaPathOptions
  diaRaw $ "\\path"
    <> element options
    <> case p of
      EmptyPath -> ""
      (Path start segs) -> element start ++ concatMap element segs
  diaRaw ";\n"


showDistance :: Constant -> String
showDistance x = showFFloat (Just 4) x tikzUnit
    where tikzUnit = "pt"

instance Element FrozenPoint where
  type Target FrozenPoint = String
  element pt = frozenPointElim pt $ \x y -> "(" <> showDistance x <> "," <> showDistance y <> ")"


-----------------
-- Path Options

localPathOptions :: (PathOptions -> PathOptions) -> Diagram a -> Diagram a
localPathOptions f = local (over diaPathOptions f)

instance Show LineTip where
  show t = case t of
    ToTip -> "to"
    SteathTip -> "stealth"
    CircleTip -> "o"
    NoTip -> ""
    LatexTip -> "latex"
    ReversedTip x -> show x ++ " reversed"
    BracketTip -> "["
    ParensTip -> "("


ultraThin, veryThin, thin, semiThick, thick, veryThick, ultraThick :: Constant
ultraThin = 0.1
veryThin = 0.2
thin = 0.4
semiThick = 0.6
thick = 0.8
veryThick = 1.2
ultraThick = 1.6


showDashPat :: DashPattern -> String
showDashPat xs = intercalate " " ["on " <> showDistance on <>
                                  " off " <> showDistance off | (on,off) <- xs]

solid             o@PathOptions{..} = o { _dashPattern = [] }
dotted            o@PathOptions{..} = o { _dashPattern = [(_lineWidth,2)] }
denselyDotted     o@PathOptions{..} = o { _dashPattern = [(_lineWidth, 1)] }
looselyDotted     o@PathOptions{..} = o { _dashPattern = [(_lineWidth, 4)] }
dashed            o@PathOptions{..} = o { _dashPattern = [(3, 3)] }
denselyDashed     o@PathOptions{..} = o { _dashPattern = [(3, 2)] }
looselyDashed     o@PathOptions{..} = o { _dashPattern = [(3, 6)] }
dashdotted        o@PathOptions{..} = o { _dashPattern = [(3, 2), (_lineWidth, 2)] }
denselyDashdotted o@PathOptions{..} = o { _dashPattern = [(3, 1), (_lineWidth, 1)] }
looselyDashdotted o@PathOptions{..} = o { _dashPattern = [(3, 4), (_lineWidth, 4)] }

using = localPathOptions
stroke color = using (outline color)
draw = stroke "black"

noOutline = set drawColor Nothing
outline color = set drawColor (Just color)

instance Element PathOptions where
  type Target PathOptions = String
  element PathOptions{..} = "["
    <> show _startTip <> "-" <> show _endTip <> ","
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
    <> "]"
    where col attr = maybe "" (\c -> attr <> "=" <> c <> ",")

----------
-- Text

drawText :: Point -> TeX -> Diagram BoxSpec
drawText point t = do
  diaRawTex $ tex $ "\\node[anchor=north west,inner sep=0] at "
  element point
  (_,box) <- diaRawTex $ inBox $ braces $ t
  diaRawTex $ tex ";\n"
  return box


