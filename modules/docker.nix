{ config, pkgs, lib, ... }:

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
        systemPackages =
          [
            {
              pkgs = with pkgs; [
                docker-compose
              ];
              rules = lib.apparmor.defaultRules;
            }
            {
              pkgs = with pkgs; [
                docker-credential-helpers
              ];

              rules = ''
                ${lib.apparmor.generateFileRules [ "pass" "gnupg" ]}
              '';
            }
          ];
      };
      virtualisation.docker = {
        enable = true;
        autoPrune.enable = true;
        daemon.settings = {
          ipv6 = true;
          fixed-cidr-v6 = "2001:db8:1::/64";
          default-address-pools = [
            {base = "10.10.0.0/16"; size = 24;}
          ];
        };
      };
      users.users.florian.extraGroups = [ "docker" ];
      home-manager.users.florian = { config, osConfig, ... }: {
        home.activation =
          {
            updateDockerCfg =
              let dockerCfgPath = "/home/florian/.docker/config.json";
                  dockerCfg = {
                    credsStore = "pass";
                  };
                  dockerCfgJSON = builtins.toJSON dockerCfg;
                  sponge = "${pkgs.moreutils}/bin/sponge";
                  jq = lib.getExe pkgs.jq;
              in lib.hm.dag.entryAfter ["writeBoundary"] ''
                run chmod u+w ${dockerCfgPath}
                run ${jq} --slurp "add" <(echo '${dockerCfgJSON}') ${dockerCfgPath} | ${sponge} ${dockerCfgPath}
              '';
          };
      };
  };

}
