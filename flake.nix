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
          pkgs = nixpkgs.legacyPackages.${system};

          disableTests = pkgs.haskell.lib.dontCheck;

          enableTests = pkgs.haskell.lib.doCheck;

          jailbreakUnbreak = pkg:
            pkgs.haskell.lib.doJailbreak (disableTests (pkgs.haskell.lib.unmarkBroken pkg));

          haskellPackages = pkgs.haskellPackages;
        in
        rec
        {
          packages.bloodhound =
            disableTests (haskellPackages.callCabal2nix "bloodhound" ./. rec {
              # Dependency overrides go here
            });
          packages.bloodhound-examples =
            haskellPackages.callCabal2nix "bloodhound-examples" ./examples rec {
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
