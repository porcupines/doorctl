{
  description = "doorctl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nfc.url = "github:centromere/nfc";
    doorctl-api.url = "git+ssh://git@github.com/porcupines/doorctl-api";
  };

  outputs = { self, nixpkgs, flake-utils, nfc, doorctl-api }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: super:
          {
            haskellPackages = super.haskellPackages.override ({
              overrides = hsSelf: hsSuper: {
                nfc = nfc.packages.${system}.nfc;
                doorctl-api = doorctl-api.packages.${system}.doorctl-api;
              };
            });
          };

        pkgs = nixpkgs.legacyPackages.${system}.appendOverlays([ overlay ]);

        haskellPackages = pkgs.haskellPackages;

        packageName = "doorctl";

        t = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;

        project = devTools:
          let addBuildTools = (t.flip hl.addBuildTools) devTools;
          in haskellPackages.developPackage {
            root = ./.;
            name = packageName;
            returnShellEnv = !(devTools == [ ]);
            modifier = (t.flip t.pipe) [
              addBuildTools
              hl.dontHaddock
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];
          };
      in {
        packages.${packageName} = project [ ];

        defaultPackage = self.packages.${system}.${packageName};

        devShell = project (with pkgs; [
          ghcid
          cabal-install
        ]);
      });
}
