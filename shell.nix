    let pkgs = (import <nixpkgs> {});
        haskellPackages = pkgs.recurseIntoAttrs(pkgs.haskellPackages.override {
            overrides = self: super:
            let callPackage = self.callPackage; in {
                  glpk-hs = callPackage nix/glpk-hs.nix {};
                  typography-geometry = callPackage nix/typography-geometry.nix {};
                  thisPackage = callPackage (import ./default.nix) {};
            };
           });
    in haskellPackages.thisPackage.env
