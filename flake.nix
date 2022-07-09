{
  description = "doorctl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-nfc.url = "github:morganthomas/nfc/nix-flake";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nfc }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: super:
          {
            haskellPackages = super.haskellPackages.override ({
              overrides = hsSelf: hsSuper: {
                haskell-nfc = haskell-nfc.packages.${system}.haskell-nfc;
              };
            });
          };

        pkgs = nixpkgs.legacyPackages.${system}.appendOverlays([ overlay ]);

        haskellPackages = pkgs.haskellPackages;

        packageName = "doorctl";
      in {
        packages.${packageName} = haskellPackages.callCabal2nix packageName ./. {};

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghcid
            cabal-install
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
