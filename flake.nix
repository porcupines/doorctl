{
  description = "doorctl";

  inputs = {
    haskell-nfc.url = "github:morganthomas/nfc/add-nix-flake";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nfc }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: super:
          {
            haskellPackages = super.haskellPackages.override ({
              overrides = hsSelf: hsSuper: {
                nfc = haskell-nfc.packages.${system}.nfc;
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
