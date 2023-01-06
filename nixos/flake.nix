{
  inputs.nixos-hardware.url = github:NixOS/nixos-hardware/master;
  # inputs.doorctl.url = github:porcupines/doorctl;

  outputs = { self, nixpkgs, nixos-hardware }@attrs: {
    nixosConfigurations.doorpi = nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      specialArgs = attrs;
      modules = [
        nixos-hardware.nixosModules.raspberry-pi-4
        ./configuration.nix
      ];
    };
  };
}
