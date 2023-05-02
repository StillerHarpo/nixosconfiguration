{ config, lib, pkgs, ... }:

{
  imports = [ ../../home/configuration.nix ];

  home.stateVersion = "22.11";
}
