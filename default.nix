{ mkDerivation, base, configurator, containers, directory, dlist
, filepath, glpk-hs, graphviz, haskell-src-exts, labeled-tree, lens
, mtl, parsek, polynomials-bernstein, pretty, process, stdenv, text
, typography-geometry, vector
}:
mkDerivation {
  pname = "marxup";
  version = "3.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base configurator containers directory dlist filepath glpk-hs
    graphviz haskell-src-exts labeled-tree lens mtl parsek
    polynomials-bernstein pretty process text typography-geometry
    vector
  ];
  description = "Markup language preprocessor for Haskell";
  license = stdenv.lib.licenses.gpl2;
}
