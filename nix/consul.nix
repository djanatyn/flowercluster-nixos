{ config, pkgs, ... }:

{
  systemd.services.consul-server = {
    description = "consul server";

    wantedBy = [ "multi-user.target" ];
    after = [ "network.target" ];

    serviceConfig = {
      ExecStart =
        "${pkgs.consul}/bin/consul agent -bind=10.0.10.3 -data-dir=/var/lib/consul.d -server";
      Restart = "always";
      User = "root";
    };
  };
}
