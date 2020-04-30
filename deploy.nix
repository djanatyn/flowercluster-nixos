let
  # Pin the deployment package-set to a specific version of nixpkgs
  pkgs = import (builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/f0da3b5e94ab0dc27ecd8f3c8ea8693a16b8c225.tar.gz";
    sha256 = "1298gihffd8ic5qvjl050m8lmcqddxnd1mxhf11imdsl3qkzy002";
  }) { };
in {
  "sahaquiel.flowercluster.io" = { config, pkgs, ... }: {
    imports = [
      <nixpkgs/nixos/modules/virtualisation/google-compute-image.nix>
      ./nix/consul.nix
      ./nix/nomad.nix
      ./nix/monitoring.nix
      ./nix/users.nix
    ];

    system.stateVersion = "18.08";

    virtualisation.docker.enable = true;

    # networking
    # ==========
    networking.hostName = "sahaquiel";
    networking.interfaces.eth0.useDHCP = true;

    networking.firewall.enable = true;
    networking.firewall.allowedUDPPorts = [ 25565 ];
    networking.firewall.allowedTCPPorts = [ 25565 ];
    services.fail2ban.enable = true;

    services.traefik.enable = true;

    # sudo
    # ====
    security.sudo.wheelNeedsPassword = false;

    # packages
    # ========
    environment.systemPackages = with pkgs; [ zsh openjdk8 consul nomad vim ];
  };
}
