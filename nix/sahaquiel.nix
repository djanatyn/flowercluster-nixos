{ config, pkgs, lib, ... }:
let
  pkgs = import (builtins.fetchGit {
    url = "https://github.com/nixos/nixpkgs.git";
    rev = "0f114432d4a9399e0b225d5be1599c7ebc5e2772";
    ref = "master";
  }) { };
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "8bbefa77f7e95c80005350aeac6fe425ce47c288";
    ref = "master";
  };
in {
  imports = [
    <nixpkgs/nixos/modules/virtualisation/google-compute-image.nix>
    (import "${home-manager}/nixos")
    <modules>
  ];

  # system state is from 18.08
  system.stateVersion = "18.08";

  # latest kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # enable postgres
  services.postgresql = { enable = true; };

  # nixpkgs
  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      terraria-server = pkgs.terraria-server.overrideAttrs (old: rec {
        version = "1.4.0.4";

        src = pkgs.fetchurl {
          url =
            "https://terraria.org/system/dedicated_servers/archives/000/000/038/original/terraria-server-1404.zip";
          sha256 = "09zkadjd04gbx1yvwpqmm89viydwxqgixbqhbqncb94qb2z5gfxk";
        };
      });
    };
  };

  # terraria: journey's end!
  services.terraria.enable = true;
  services.terraria.password = lib.fileContents /var/secrets/terraria-password;

  # enable docker
  virtualisation.docker.enable = true;

  # networking
  # ==========
  networking.hostName = "sahaquiel";
  networking.interfaces.eth0.useDHCP = true;

  networking.firewall.enable = true;
  networking.firewall.allowedUDPPorts = [ 25565 7777 8388 ];
  networking.firewall.allowedTCPPorts = [ 25565 7777 8388 ];
  services.fail2ban.enable = true;

  services.traefik.enable = true;

  # sudo
  # ====
  security.sudo.wheelNeedsPassword = false;

  # packages
  # ========
  environment.systemPackages = with pkgs; [
    zsh
    openjdk8
    consul
    nomad
    vim
    exa
    git
    python
  ];
}
