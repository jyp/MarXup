{-# LANGUAGE RecordWildCards #-}
module MarXup.Diagram.Graphviz where

import MarXup.Diagram.Layout
import Data.GraphViz
import Data.GraphViz.Parsing
import Data.GraphViz.Commands.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.Traversable
import Data.Foldable

layout :: (Ord n, PrintDot n, ParseDot n) => GraphvizCommand -> DotGraph n -> DotGraph n
layout cmd input = parseIt' $ unsafePerformIO $ graphvizWithHandle cmd input DotOutput hGetStrict 

graphToDiagram :: DotGraph Int -> Dia
graphToDiagram (DotGraph _strict _directed _grIdent (DotStmts{..})) =
  forM_ nodeStmts $ \(DotNode nodeIdent nodeAttrs) -> do
    error "todo"
    
  


