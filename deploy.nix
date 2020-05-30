let
  # Pin the deployment package-set to a specific version of nixpkgs
  pkgs = import (builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/f0da3b5e94ab0dc27ecd8f3c8ea8693a16b8c225.tar.gz";
    sha256 = "1298gihffd8ic5qvjl050m8lmcqddxnd1mxhf11imdsl3qkzy002";
  }) { };
in {
  "sahaquiel.flowercluster.io" = { config, pkgs, lib, ... }: {
    imports = [
      <nixpkgs/nixos/modules/virtualisation/google-compute-image.nix>
      ./nix/consul.nix
      ./nix/nomad.nix
      ./nix/monitoring.nix
      ./nix/users.nix
    ];

    # system state is from 18.08
    system.stateVersion = "18.08";

    # morph deployment secrets
    deployment = {
      secrets = {
        "terraria-password" = {
          source = "/var/secrets/terraria-password";
          destination = "/var/secrets/terraria-password";
          owner.user = "terraria";
          owner.group = "terraria";
          permissions = "0600";
        };
      };
    };

    # latest kernel
    boot.kernelPackages = pkgs.linuxPackages_latest;

    # nixpkgs
    nixpkgs.config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        terraria-server = pkgs.terraria-server.overrideAttrs (old: rec {
          version = "1.4.0.2";

          src = pkgs.fetchurl {
            url =
              "https://terraria.org/system/dedicated_servers/archives/000/000/037/original/terraria-server-1403.zip";
            sha256 = "1g9rd0a40gsljk8xp3bkvwy8ngywjzk8chf2x9l43s2kf40ib0p8";
          };
        });
      };
    };

    # terraria: journey's end!
    services.terraria.enable = true;
    services.terraria.password =
      lib.fileContents /var/secrets/terraria-password;

    # enable docker
    virtualisation.docker.enable = true;

    # dante socks5 proxy
    services.dante.enable = true;
    services.dante.config = ''
      # socksmethod: none // for non-authentication
      socksmethod: username

      socks pass {
              from: 0.0.0.0/0 to: 0.0.0.0/0
              command: bind connect udpassociate
              log: error connect disconnect
              socksmethod: username
      }
    '';

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
    ];
  };
}
