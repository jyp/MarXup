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

layout :: (Ord n, PrintDot n, ParseDot n) => GraphvizCommand -> DotGraph n -> Gen.DotGraph n
layout command input = parseIt' $ unsafePerformIO $ graphvizWithHandle command input DotOutput hGetStrict 

pos (Pos p) = Just p
pos _= Nothing

shapeA (Shape s) = Just s
shapeA _ = Nothing

widthA (Width s) = Just s
widthA _ = Nothing

labelA (Label l) = Just l
labelA _ = Nothing

readAttr :: Monad m => (Attribute -> Maybe a) -> [Attribute] -> (a -> m ()) -> m ()
readAttr f as k = readAttr' f as k (return ())

readAttr' :: (Attribute -> Maybe a) -> [Attribute] -> (a -> k) -> k -> k
readAttr' f as k1 k2 = case [x | Just x <- map f as] of
  (x:_) -> k1 x
  _ -> k2

graph :: (Ord n, PrintDot n, ParseDot n, Show n) => GraphvizCommand -> DotGraph n -> Dia
graph cmd gr = graphToDiagram $ layout cmd gr

pt' (G.Point x y _z _forced) = D.Point x y
pt = unfreeze . pt'

-- diaSpline [w,x,y,z] = Path (pt w) [CurveTo (pt x) (pt y) (pt z)]
diaSpline [w,x,y,z] = [curve w x y z]

graphToDiagram :: Gen.DotGraph n -> Dia
graphToDiagram (Gen.DotGraph _strict _directed _grIdent stmts) = do
  forM_ stmts $ \ stmt -> case stmt of
    (Gen.DE (DotEdge _from _to attrs)) -> do
      diaRaw $ "%Edge: " ++ show attrs ++ "\n"
      readAttr pos attrs $ \(SplinePos splines) ->
        forM_ splines $ \Spline{..} -> do
          case startPoint of
            Just p -> draw $ path $ circle (pt p) 4
            Nothing -> return ()
          case endPoint of
            Just p -> draw $ path $ circle (pt p) 4
            Nothing -> return ()
          draw $ frozenPath $ fromBeziers $ diaSpline $ map pt' splinePoints

    (Gen.DN (DotNode _nodeIdent attrs)) -> do
      diaRaw $ "%Node: " ++ show attrs ++ "\n"
      readAttr pos   attrs $ \(PointPos p) -> do
        readAttr labelA attrs $ \l -> do
          case l of
            StrLabel l -> do
              l' <- labelObj $ tex $ T.unpack $ l
              l' # D.Center .=. pt p
        readAttr widthA attrs $ \w ->
         readAttr shapeA attrs $ \s ->
          case s of
            Circle -> do
              draw $ path $ circle (pt p) (constant $ inch (w/2))
            _ -> return ()
    _ -> return ()

inch x = 72 * x


