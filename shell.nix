let
  # cure = x: pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.doJailbreak x);
  haskellOverlay = self: super: {
    haskellPackages = super.haskell.packages.ghc922.override {
      overrides = hself: hsuper: rec {
        ghc-lib-parser = hsuper.ghc-lib-parser_9_2_2_20220307;
      };
    };
  };
in
{ pkgs ? import <nixpkgs> { overlays = [ haskellOverlay ]; } }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskellPackages.ormolu_0_5_0_0
  ];
}
