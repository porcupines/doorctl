{
  description = "doorctl";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    doorctl-api = {
      url = "git+ssh://git@github.com/porcupines/doorctl-api";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, nixpkgs, flake-utils, doorctl-api }:
    flake-utils.lib.eachSystem [
      flake-utils.lib.system.aarch64-linux
      # flake-utils.lib.system.x86_64-linux
    ] (system:
      let
        overlay = final: prev: {
          haskellPackages = prev.haskell.packages.ghc944.override {
            overrides = hsFinal: hsPrev: {
              cborg = prev.haskellPackages.cborg;
              doorctl-api = doorctl-api.packages.${system}.doorctl-api-controller;

              servant = final.haskell.lib.overrideCabal hsPrev.servant (old: {
                doHaddock = false;
                doCheck = false; # Depends on hspec <2.10, and nixpkgs is newer
              });

              servant-client = final.haskell.lib.overrideCabal hsPrev.servant-client (old: {
                doHaddock = false;
                doCheck = false; # Depends on hspec <2.10, and nixpkgs is newer
              });
              servant-client-core = final.haskell.lib.overrideCabal hsPrev.servant-client-core (old: {
                doHaddock = false;
                doCheck = false; # Depends on hspec <2.10, and nixpkgs is newer
              });

              servant-server = final.haskell.lib.overrideCabal hsPrev.servant-server (old: {
                doHaddock = false;
                doCheck = false; # Depends on hspec <2.10, and nixpkgs is newer
              });
            };
          };
        };

        pkgs = nixpkgs.legacyPackages.${system}.appendOverlays([
          doorctl-api.overlays.${system}.default
          overlay
        ]);

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
              hl.disableExecutableProfiling
              hl.linkWithGold
              hl.justStaticExecutables
              hl.enableStaticLibraries
            ];

            source-overrides = {
              attoparsec-iso8601 = "1.1.0.0";
              http-api-data = "0.5";
            };
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
