let
  krops = builtins.fetchGit {
    url = "https://cgit.krebsco.de/krops/";
    ref = "v1.21.0";
    rev = "55aa2c77ce8183f3d2b24f54efa33ab6a42e1e02";
  };
  lib = import "${krops}/lib";
  pkgs = import "${krops}/pkgs" { };

  sources = {
    sahaquiel = lib.evalSource [{
      nixpkgs.git = {
        url = "https://github.com/nixos/nixpkgs.git";
        ref = "master";
      };
      nixos-config.file = toString ./nix/sahaquiel.nix;
      modules.file = toString ./nix/modules;
    }];
  };
in {
  sahaquiel = pkgs.krops.writeDeploy "deploy-sahaquiel" {
    source = sources.sahaquiel;
    target = "root@sahaquiel.flowercluster.io";
  };
}
