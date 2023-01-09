{ config, doorctl, pkgs, lib, ... }:

let
  hostname = "doorpi";
in {
  nix = {
    settings = {
      allowed-users = [ "*" ];
      builders-use-substitutes = true;
      experimental-features = [ "nix-command" "flakes" ];
      http2 = true;
      trusted-users = [ "root" "alex" ];
    };
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  networking = {
    hostName = hostname;
  };

  environment.systemPackages = with pkgs; [
    cachix
    doorctl.packages.aarch64-linux.doorctl
    git
    libnfc
    tmux
    vim
  ];

  systemd.services.doorctl = {
    unitConfig = {
      Description = "Door Control Service";
      Requires = "network-online.target";
      After = "network-online.target";
    };

    serviceConfig = {
      Restart = "always";
      RestartSec = 2;
      ExecStartPre = [
        "/run/current-system/sw/bin/stty -F /dev/vestibule_reader 115200"
        "/run/current-system/sw/bin/stty -F /dev/basement_reader 115200"
        "/run/current-system/sw/bin/stty -F /dev/stairs_reader 115200"
      ];
      ExecStart = "${doorctl.packages.aarch64-linux.doorctl}/bin/doorctl /etc/doorctl.conf";
      KillSignal = "SIGKILL";
    };

    restartTriggers = [
      doorctl.packages.aarch64-linux.doorctl
    ];

    wantedBy = [ "multi-user.target" ];
  };

  services = {
    openssh = {
      enable = true;
    };

    udev = {
      extraRules = ''
        SUBSYSTEM=="tty", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6001", ATTRS{serial}=="AK06VDZD", SYMLINK+="vestibule_reader"
        SUBSYSTEM=="tty", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6001", ATTRS{serial}=="AL02UTIO", SYMLINK+="stairs_reader"
        SUBSYSTEM=="tty", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6001", ATTRS{serial}=="AL02UZVC", SYMLINK+="basement_reader"
      '';
    };
  };

  boot = {
    tmpOnTmpfs = true;
  };

  users = {
    mutableUsers = false;

    users.alex = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      hashedPassword = "...";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMwRlnugNqDq9supcWaI5ghYImvCsmfDew6381I/4wmE alex@hayek"
      ];
    };

    users.morgan = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      hashedPassword = "...";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINR56SZ7Xy3IuqE6+vPxvbTEZnM+owSTdQ+NP/Pj/yUy morgan@godel"
      ];
    };
  };

  system.stateVersion = "22.05";
}
