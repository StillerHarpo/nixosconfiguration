{
  config,
  pkgs,
  lib,
  ...
}:

with lib;

let
  cfg = config.docker;
in

{
  options.docker = {
    enable = mkEnableOption (lib.mdDoc "Whether to use docker");
  };
  config = mkIf cfg.enable {
    apparmor = {
      systemPackages = [
        {
          pkgs = with pkgs; [
            docker-compose
          ];
          rules = lib.apparmor.defaultRules config;
        }
        {
          pkgs = with pkgs; [
            docker-credential-helpers
          ];

          rules = ''
            ${lib.apparmor.generateFileRules {
              allows = [
                "pass"
                "gnupg"
              ];
              inherit config;
            }}
          '';
        }
      ];
    };
    virtualisation.docker = {
      enable = true;
      autoPrune.enable = true;
    };
    users.users.florian.extraGroups = [ "docker" ];
    home-manager.users.florian =
      { config, osConfig, ... }:
      {
        home.activation = {
          updateDockerCfg =
            let
              dockerCfgPath = "/home/florian/.docker/config.json";
              dockerCfg = {
                credsStore = "pass";
              };
              dockerCfgJSON = builtins.toJSON dockerCfg;
              sponge = "${pkgs.moreutils}/bin/sponge";
              jq = lib.getExe pkgs.jq;
            in
            lib.hm.dag.entryAfter [ "writeBoundary" ] ''
              run chmod u+w ${dockerCfgPath}
              run ${jq} --slurp "add" <(echo '${dockerCfgJSON}') ${dockerCfgPath} | ${sponge} ${dockerCfgPath}
            '';
        };
      };
  };

}
