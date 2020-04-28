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

    # users
    # =====
    security.sudo.wheelNeedsPassword = false;

    users.users.djanatyn = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "docker" ];
      shell = pkgs.zsh;

      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDFQPTKrT397qtitl0hHkl3HysPfnpEm/WmO9f4dC4kLkrHIgs2t9Yvd6z+8C/hufW+e0cVug3sb6xHWFI78+/eCSRQpPWVsE3e6/U5R/EGJqylPLEa/SmB4hB6LpsCnJkeHnD/sVBz/EjFD29wifLFq0Y5keMdxbvUMjkGrep0CD1guYseFJOdFpLF3A5GAnnP2CHgvOT7/Pd2mym5f2Mxp17SF1iYAsx9xId5o6YbmKldz3BN51N+9CROSg9QWuSNCvA7qjflBIPtnBVZFvIN3U56OECZrv9ZY4dY2jrsUGvnGiyBkkdxw4+iR9g5kjx9jPnqZJGSEjWOYSl+2cEQGvvoSF8jPiH8yLEfC+CyFrb5FMbdXitiQz3r3Xy+oLhj8ULhnDdWZpRaJYTqhdS12R9RCoUQyP7tlyMawMxsiCUPH/wcaGInzpeSLZ5BSzVFhhMJ17TX+OpvIhWlmvpPuN0opmfaNGhVdBGFTNDfWt9jjs/OHm6RpVXacfeflP62xZQBUf3Hcat2JOqj182umjjZhBPDCJscfv52sdfkiqwWIc/GwdmKt5HqU+dX7lCFJ1OGF2ymnGEnkUwW+35qX8g2P+Vc4s28MmaO5M1R5UsMFnhtFbLdfLFKn2PEvepvIqyYFMziPzEBya4zBUch/9sd6UN3DV+rA/JB/rBApw== djanatyn@nixos"
      ];
    };
  };
}
