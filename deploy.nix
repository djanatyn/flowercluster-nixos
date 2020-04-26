let
  # Pin the deployment package-set to a specific version of nixpkgs
  pkgs = import (builtins.fetchTarball {
    url =
      "https://github.com/nixos/nixpkgs/archive/f0da3b5e94ab0dc27ecd8f3c8ea8693a16b8c225.tar.gz";
    sha256 = "1298gihffd8ic5qvjl050m8lmcqddxnd1mxhf11imdsl3qkzy002";
  }) { };
in {
  "sahaquiel.flowercluster.io" = { config, pkgs, ... }: {
    imports =
      [ <nixpkgs/nixos/modules/virtualisation/google-compute-image.nix> ];

    networking.hostName = "sahquiel";

    system.stateVersion = "18.08";

    virtualisation.docker.enable = true;

    environment.systemPackages = with pkgs; [ zsh openjdk8 nomad vim ];

    systemd.services.minecraft-eternal = {
      description = "Minecraft Eternal 1.3.5 Server";

      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        ExecStart =
          "${pkgs.openjdk8}/bin/java -Xmx5120M -Xms5120M -Dfml.queryResult=confirm -Dfml.readTimeout=120 -jar forge-1.12.2-14.23.5.2847-universal.jar nogui";
        Restart = "always";
        User = "minecraft";
        WorkingDirectory = /opt/eternal-lite-1.3.5;
      };
    };

    networking.firewall.allowedUDPPorts = [ 25565 ];
    networking.firewall.allowedTCPPorts = [ 25565 ];

    users.users.minecraft = { isNormalUser = true; };

    environment.etc."nomad-server.hcl".text = ''
      data_dir  = "/var/lib/nomad"

      server {
        enabled = true
        bootstrap_expect = 1
      }
    '';

    systemd.services.nomad-server = {
      description = "Hashicorp Nomad Server";

      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        ExecStart =
          "${pkgs.nomad}/bin/nomad agent -config /etc/nomad-server.hcl";
        Restart = "always";
        User = "root";
      };
    };
  };
}