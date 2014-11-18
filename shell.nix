let pkgs = (import <nixpkgs> {});
    haskellPackages = pkgs.haskellPackages;
    myPkg = haskellPackages.callPackage (import ./default.nix) {};
in pkgs.myEnvFun {
    name = myPkg.name;
    buildInputs = 
       [(haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ myPkg.propagatedNativeBuildInputs)))];
     }
