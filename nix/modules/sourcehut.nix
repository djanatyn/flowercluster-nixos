{ config, lib, pkgs, ... }:
with lib;
let cfg = config.flowercluster.services.sourcehut;
in {
  options = {
    flowercluster.services.sourcehut.enable = mkEnableOption "sourcehut";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      sourcehut.gitsrht
      sourcehut.todosrht
      sourcehut.mansrht
      sourcehut.metasrht
      sourcehut.pastesrht
      sourcehut.dispatchsrht
      sourcehut.buildsrht
    ];
  };
}
