{ config, pkgs, ... }:

{
  services.grafana.enable = true;

  services.prometheus.enable = true;
  services.prometheus.exporters.node.enable = true;
  services.prometheus.scrapeConfigs = [{
    job_name = "node_scraper";
    static_configs = [{
      targets = [
        "${toString config.services.prometheus.exporters.node.listenAddress}:${
          toString config.services.prometheus.exporters.node.port
        }"
      ];
    }];
  }];
}
