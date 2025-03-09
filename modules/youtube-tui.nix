{ config, lib, inputs, ... }:

let
  cfg = config.youtube-tui;

in

{
  options.youtube-tui = {
    enable = lib.mkEnableOption (lib.mdDoc "Enable youtube-tui");
  };
  config = lib.mkIf cfg.enable {
      apparmor = {
        systemPackages = [
          { pkgs = [ inputs.nixpkgs-master.legacyPackages."${config.nixpkgs.hostPlatform.system}".youtube-tui ];
            rules = lib.apparmor.defaultRules;
          }];
      };
      home-manager.users.florian.xdg.configFile."youtube-tui/main.yml".text =
        lib.generators.toYAML { } {
          invidious_instance = "https://yt.artemislena.eu";
        };
  };
}
