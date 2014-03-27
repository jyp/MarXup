module MarXup.Diagram.Graphviz where


import Data.GraphViz
import Data.GraphViz.Parsing

layout :: (Ord n, PrintDot n, ParseDot n) => GraphvizCommand -> DotGraph n -> DotGraph n
layout cmd input = case unsafePerformIO $ graphvizWithHandle cmd input DotOutput hGetContents' of
  Left err -> error  $ "Grapviz layout: graphviz reports: " ++ show err
  Right out -> parse out


