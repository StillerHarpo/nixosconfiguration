{ config, lib, pkgs, ... }:

{
  imports = [ ../../home/configuration.nix ];

  xsession.windowManager.xmonad.config = ./xmonad.hs;
}
