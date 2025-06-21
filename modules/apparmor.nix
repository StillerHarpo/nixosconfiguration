{ config, lib, ... }:

let cfg = config.apparmor;

in {
  options = {
    apparmor.enable = lib.mkEnableOption (lib.mdDoc "Wether to use apparmor");
    apparmor.systemPackages = lib.mkOption {
      type = lib.types.listOf (lib.types.submodule ({...}: {
        options = {
          pkgs = lib.mkOption {
            type = lib.types.listOf lib.types.package;
            description = lib.mdDoc "Packages with the same profile";
          };
          rules = lib.mkOption {
            description = lib.mdDoc "The apparmor rules for the packages.";
            type = lib.types.lines;
          };
        };
      }));
      default = [];
      description = lib.mdDoc "the system packages together with their apparmor profiles";

    };
  };

  config = lib.mkMerge
    [
      {
        environment.systemPackages = lib.concatMap (p: p.pkgs) cfg.systemPackages;
      }
      (
        lib.mkIf (cfg.enable == true)
          {
            security = {
              apparmor = {
                enable = true;
                policies.allRules.profile =
                  let newlineSepMap = lib.strings.concatMapStringsSep "\n"; 
                  in newlineSepMap (
                    rulePkgs: newlineSepMap (
                      pkg: ''
                        ${pkg}/bin/* {
                           ${rulePkgs.rules}
                         }
                       ''
                    ) rulePkgs.pkgs
                  ) cfg.systemPackages;
              };
              auditd.enable = true;
            };
            services = {
              syslogd = {
                enable = true;
                extraConfig = ''
                   *.*  -/var/log/syslog
                 '';
              };
              journald.forwardToSyslog = true;
            };
          }
      )
    ];

} 
