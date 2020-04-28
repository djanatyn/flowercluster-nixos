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
    ];

    system.stateVersion = "18.08";

    virtualisation.docker.enable = true;

    # networking
    # ==========
    networking.hostName = "sahaquiel";
    networking.firewall.enable = true;
    networking.firewall.allowedUDPPorts = [ 25565 ];
    networking.firewall.allowedTCPPorts = [ 25565 ];
    services.fail2ban.enable = true;

    # monitoring
    # ==========
    services.grafana.enable = true;
    services.prometheus.enable = true;
    services.prometheus.exporters.node.enable = true;
    services.prometheus.scrapeConfigs = [{
      job_name = "node_scraper";
      static_configs = [{
        targets = [
          "${
            toString config.services.prometheus.exporters.node.listenAddress
          }:${toString config.services.prometheus.exporters.node.port}"
        ];
      }];
    }];

    # packages
    # ========
    environment.systemPackages = with pkgs; [ zsh openjdk8 consul nomad vim ];
  };
}
