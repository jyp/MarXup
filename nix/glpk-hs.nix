{ mkDerivation, array, base, containers, deepseq, fetchgit, glpk
, mtl, stdenv
}:
mkDerivation {
  pname = "glpk-hs";
  version = "0.3.4";
  src = fetchgit {
    url = "https://github.com/jyp/glpk";
    sha256 = "bd0ab93f858c87e0bf6eb80bd11df9be25c482cc475e780fe7b31722e2843f5f";
    rev = "54bf1b6bad176bf9a2dff417e63eedfd63293a07";
  };
  buildDepends = [ array base containers deepseq mtl ];
  extraLibraries = [ glpk ];
  description = "Comprehensive GLPK linear programming bindings";
  license = stdenv.lib.licenses.bsd3;
}
