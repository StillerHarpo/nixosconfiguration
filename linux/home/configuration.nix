{ config, lib, pkgs, ... }:

{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
}
