{ config, lib, pkgs, ... }:

{
  require = [ ./consul.nix ./monitoring.nix ./nomad.nix ./users.nix ];
}
