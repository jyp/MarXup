{-# LANGUAGE RecordWildCards #-}
module MarXup.Diagram.Graphviz (graph) where

import MarXup.Tex
import MarXup.Diagram.Point as D
import MarXup.Diagram.Object as D
import MarXup.Diagram.Tikz as D
import MarXup.Diagram.Layout
import MarXup.Diagram.Path
import Data.GraphViz as G
import Data.GraphViz.Attributes.Complete as G
import Data.GraphViz.Parsing as G
import qualified Data.GraphViz.Types.Generalised as Gen
import Data.GraphViz.Commands.IO as G
-- import qualified Data.Text.Lazy.Internal as T
import qualified Data.Text.Lazy as T
import System.IO.Unsafe (unsafePerformIO)
-- import Data.Traversable
import Data.Foldable
import Control.Lens (set)

graph :: (PrintDotRepr g n, ParseDot n, PrintDot n) => GraphvizCommand -> g n -> Dia
graph cmd gr = graphToDiagram $ layout cmd gr

layout :: (PrintDotRepr g n, ParseDot n, PrintDot n) => GraphvizCommand -> g n -> Gen.DotGraph n
layout command input = parseIt' $ unsafePerformIO $ graphvizWithHandle command input DotOutput hGetStrict 

pos (Pos p) = Just p
pos _= Nothing

lpos (LPos p) = Just p
lpos _= Nothing

shapeA (Shape s) = Just s
shapeA _ = Nothing

widthA (Width s) = Just s
widthA _ = Nothing

labelA (Label l) = Just l
labelA _ = Nothing

arrowHeadA (ArrowHead a) = Just a
arrowHeadA _ = Nothing

readAttr :: Monad m => (Attribute -> Maybe a) -> [Attribute] -> (a -> m ()) -> m ()
readAttr f as k = readAttr' f as k (return ())

readAttr' :: (Attribute -> Maybe a) -> [Attribute] -> (a -> k) -> k -> k
readAttr' f as k1 k2 = case [x | Just x <- map f as] of
  (x:_) -> k1 x
  _ -> k2


pt' (G.Point x y _z _forced) = D.Point x y
pt = unfreeze . pt'

diaSpline (w:x:y:z:rest) = curveSegment w x y z:diaSpline (z:rest)
diaSpline _ = []

-- ToTip | CircleTip | NoTip | StealthTip | LatexTip | ReversedTip LineTip | BracketTip | ParensTip
tipTop def (AType [(_,NoArrow)]) = NoTip
tipTop def (AType [(_,Normal)]) = LatexTip
tipTop def (AType [(_,DotArrow)]) = CircleTip
tipTop def (AType [(_,Vee)]) = StealthTip
tipTop def _ = def

renderLab l p = do
  l' <- labelObj $ tex $ T.unpack $ l
  l' # D.Center .=. pt p

graphToDiagram :: Gen.DotGraph n -> Dia
graphToDiagram (Gen.DotGraph _strict _directed _grIdent stmts) = do
  forM_ stmts $ \ stmt -> case stmt of
    (Gen.DE (DotEdge _from _to attrs)) -> do
      diaRaw $ "%Edge: " ++ show attrs ++ "\n"
      let toTip = readAttr' arrowHeadA attrs (tipTop ToTip) ToTip
      readAttr labelA attrs $ \(StrLabel l) ->
        readAttr lpos attrs $ \p -> 
        renderLab l p
      readAttr pos attrs $ \(SplinePos splines) ->
        forM_ splines $ \Spline{..} -> do
          let mid = diaSpline $ map pt' splinePoints
          let beg = case (startPoint,splinePoints) of
                (Just p,q:_) -> [lineSegment (pt' p) (pt' q)]
                _ -> []
          let end = case (endPoint,reverse splinePoints) of
                (Just p,q:_) -> [lineSegment (pt' q) (pt' p)]
                _ -> []
          using (set endTip toTip) $
            draw $ frozenPath' $ fromBeziers (beg ++ mid ++ end)

    (Gen.DN (DotNode _nodeIdent attrs)) -> do
      diaRaw $ "%Node: " ++ show attrs ++ "\n"
      readAttr pos   attrs $ \(PointPos p) -> do
        readAttr labelA attrs $ \l -> do
          case l of
            StrLabel l -> renderLab l p
        readAttr widthA attrs $ \w ->
         readAttr shapeA attrs $ \s ->
          case s of
            Circle -> do
              draw $ path $ circle (pt p) (constant $ inch (w/2))
            _ -> return ()
    _ -> return ()

inch x = 72 * x


