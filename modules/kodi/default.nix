{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.kodi;

in

{
  options.kodi = {
    enable = mkEnableOption (lib.mdDoc "Whether to install kodi with plugins");
  };
  config = mkIf cfg.enable ({
    apparmor.systemPackages = [
      {
        pkgs = with pkgs; [
          (kodi.withPackages (kodiPkgs:
            with kodiPkgs; [
              netflix
              steam-controller
              invidious
              arteplussept
            ]))
        ];
        rules = lib.apparmor.defaultRules;
      }
    ];

    home-manager.users.florian = {
      home.file = {
        ".kodi/userdata/addon_data/plugin.video.invidious/settings.xml".source = ./invidious.xml;
        ".kodi/userdata/addon_data/plugin.video.arteplussept/settings.xml".source = ./arteplussept.xml;
        ".kodi/userdata/addon_data/skin.estuary/settings.xml".source = ./estuary.xml;
        ".kodi/userdata/sources.xml".source = ./sources.xml;
      };
    };
  });
}
