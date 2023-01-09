{
  inputs.nixos-hardware.url = github:NixOS/nixos-hardware/master;
  inputs.doorctl.url = "git+ssh://git@github.com/porcupines/doorctl";

  outputs = { self, nixpkgs, nixos-hardware, doorctl }@attrs: {
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
