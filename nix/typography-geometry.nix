{ mkDerivation, base, containers, fetchgit, parallel
, polynomials-bernstein, stdenv, vector
}:
mkDerivation {
  pname = "typography-geometry";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/jyp/typography-geometry";
    sha256 = "7898d1dec908fb13daa12e59c330bf625fec5d9430388589b02b28888b4add14";
    rev = "534501ac02a006e5998c1fec5be46369eb89bb6c";
  };
  buildDepends = [
    base containers parallel polynomials-bernstein vector
  ];
  description = "Drawings for printed text documents";
  license = "GPL";
}
