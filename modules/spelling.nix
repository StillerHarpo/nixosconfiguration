{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.spelling;
in
{
  options.spelling = {
    enable = lib.mkEnableOption (lib.mdDoc "Enable spelling");
  };
  config = lib.mkIf cfg.enable {
    apparmor.systemPackages = [
      {
        pkgs = [ pkgs.myAspellWithDicts ];
        rules = lib.apparmor.defaultRules config;
      }
    ];
    home-manager.sharedModules = [
      {
        home.file.".aspell.conf".source = pkgs.runCommand "aspell.conf" { } ''
          ${lib.getExe' pkgs.myAspellWithDicts "aspell"} dump config > $out
        '';
      }
    ];
  };
}
