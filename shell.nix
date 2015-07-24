    let pkgs = (import <nixpkgs> {});
        haskellPackages = pkgs.recurseIntoAttrs(pkgs.haskellPackages.override {
            overrides = self: super:
            let callPackage = self.callPackage; in {
                  glpk-hs = callPackage nix/glpk-hs.nix {};
                  # glpk-hs = callPackage /home/bernardy/repo/glpk/default.nix {};
                  typography-geometry = callPackage nix/typography-geometry.nix {};
                  thisPackage = callPackage (import ./default.nix) {};
            };
           });
    in pkgs.stdenv.mkDerivation {
    name = haskellPackages.thisPackage.name;
    buildInputs = 
       [(haskellPackages.ghcWithPackages (hs: ([
         hs.cabal-install
         hs.hdevtools
         hs.alex
         hs.happy
         hs.hscolour
       ] ++ haskellPackages.thisPackage.propagatedNativeBuildInputs)))];
    shellHook = ''
      export LANG=en_US.utf8
      '';
     }
