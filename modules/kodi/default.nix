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
              # FIXME Remove this when https://github.com/NixOS/nixpkgs/pull/215792 is in stable
              (kodiPkgs.invidious.overrideAttrs (_: {
                src = fetchFromGitHub {
                  owner = "TheAssassin";
                  repo = "kodi-invidious-plugin";
                  rev = "85b66525632d94630c9301d9c490fc002a335d77";
                  hash =
                    "sha256-DpsAQUOUYCs3rpWwsk82+00KME4J+Iocu/v781dyyws=";
                };
              }))
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
