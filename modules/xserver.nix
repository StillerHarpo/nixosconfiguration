{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.xserver;
in
{
  options.xserver = {
    enable = mkEnableOption (lib.mdDoc "Wether to use xserver");
    autoLogin = mkEnableOption (lib.mdDoc "Wether to autologin into wayland");
  };
  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      xkbOptions = "eurosign:e, caps:escape, grp:alt_shift_toggle";
      xkbVariant = "altgr-intl";
      monitorSection = ''Option "DPMS" "false"'';
      serverLayoutSection = ''
        Option          "BlankTime"     "0"
        Option          "StandbyTime"   "0"
        Option          "SuspendTime"   "0"
        Option          "OffTime"       "0"
      '';
      # Enable XMonad
      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
      };
      displayManager = mkIf cfg.autoLogin {
        defaultSession = "none+xmonad";
        autoLogin = {
          enable = true;
          user = "florian";
        };
      };
    };
  };
}
