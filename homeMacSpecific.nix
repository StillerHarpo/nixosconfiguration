{ config, lib, pkgs, ... }:

{
  imports = [
    ./homeCommon.nix
    ./zshMac.nix
  ];
}
