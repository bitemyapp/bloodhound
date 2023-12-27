{
  description = "bloodhound";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; };

          disableTests = pkgs.haskell.lib.dontCheck;

          enableTests = pkgs.haskell.lib.doCheck;

          haskellPackages = pkgs.haskellPackages;
        in
        rec
        {
          packages.bloodhound =
            disableTests (haskellPackages.callCabal2nix "bloodhound" ./. {
              # Dependency overrides go here
            });
          packages.bloodhound-examples =
            haskellPackages.callCabal2nix "bloodhound-examples" ./examples {
              # Dependency overrides go here
              bloodhound = packages.bloodhound;
            };

          defaultPackage = packages.bloodhound;

          devShell =
            let
              scripts = pkgs.symlinkJoin {
                name = "scripts";
                paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin {
                  ormolu-ide = ''
                    ${pkgs.ormolu}/bin/ormolu -o -XNoImportQualifiedPost $@
                  '';
                };
              };
            in
            pkgs.mkShell {
              buildInputs = with haskellPackages; [
                haskell-language-server
                ghcid
                cabal-install
                scripts
                ormolu
              ];
              inputsFrom = [
                (enableTests self.defaultPackage.${system}).env
              ];
            };
        });
}
