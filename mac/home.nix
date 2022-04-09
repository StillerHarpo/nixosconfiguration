{ config, lib, pkgs, ... }:
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
  config.home.file."Applications/Home Manager Apps".source = "${appEnv}/Applications";
}
