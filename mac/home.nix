{ config, lib, pkgs, home-manager, ... }:
let
  appEnv = pkgs.buildEnv {
    name = "home-manager-applications";
    paths = config.home.packages;
    pathsToLink = "/Applications";
  };
in
{
  imports = [
    ../home.nix
    ./zsh.nix
  ];
  config.home = {
    stateVersion = "22.05";
    file."Applications/Home Manager Apps".source = "${appEnv}/Applications";
  };
}
