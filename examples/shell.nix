let pkgs = (import <nixpkgs> {});
    haskellPackages = pkgs.recurseIntoAttrs(pkgs.haskellPackages.override {
        overrides = self: super:
        let callPackage = self.callPackage; in {
              glpk-hs = callPackage ../nix/glpk-hs.nix {};
              typography-geometry = callPackage ../nix/typography-geometry.nix {};
              marxup = callPackage (import ../default.nix) {};
        };
       });
    myGhc = haskellPackages.ghcWithPackages (haskellPackages: with haskellPackages; [
              marxup]);

in pkgs.runCommand "dummy" { buildInputs = [ myGhc ]; } ""


