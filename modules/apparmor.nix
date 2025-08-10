{ config, lib, pkgs, ... }:

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
                policies = {
                  scripts.profile = ''
                    /home/** {
                       ${lib.apparmor.generateFileRules []}
                    }
                    /home/florian/meqo/checkpad-ssh/* {
                       ${lib.getExe pkgs.openssh} Px,
                       ${lib.getExe' pkgs.openssh "scp"} Px,
                       ${lib.getExe' pkgs.openssh "ssh-copy-id"} Px,
                       ${lib.getExe' pkgs.openssh "sshfs"} Px,
                       ${lib.apparmor.generateFileRules []}
                    }
                    /home/florian/meqo/meqo/server/prepare {
                       network,
                       ${lib.apparmor.generateFileRules []}
                    }
                    /home/florian/meqo/meqo/server/scripts/make-release {
                       ${lib.getExe pkgs.git} Px -> gitnodocs,
                       ${lib.apparmor.generateFileRules []}
                    }
                    /home/florian/Dokumente/scans/generateTxts.sh {
                       ${lib.apparmor.generateFileRules [ "docs" ]}
                    }
                  '';
                  specialRules.profile = ''
                       ${lib.getExe pkgs.git} {
                         ${lib.getExe pkgs.gnupg} Px -> gnupgnonet,
                         ${lib.getExe pkgs.openssh} Px,
                         network,
                         ${lib.apparmor.generateFileRules [ "docs"]}
                       } 
                       profile gitnodocs {
                         ${lib.getExe pkgs.gnupg} Px -> gnupgnonet,
                         ${lib.getExe pkgs.openssh} Px,
                         network,
                         ${lib.apparmor.generateFileRules [ ]}
                       } 
                       ${lib.getExe pkgs.gnupg} {
                          ${lib.apparmor.generateFileRules [ "gnupg" ]}
                          network,
                       }
                       profile gnupgnonet {
                          ${lib.apparmor.generateFileRules [ "gnupg" ]}
                       }
                       ${lib.getExe pkgs.openssh} {
                         ${lib.apparmor.generateFileRules [ "ssh" ]}
                         network,
                         capability,
                       }
                       ${lib.getExe' pkgs.openssh "scp"} {
                         ${lib.apparmor.generateFileRules [ "ssh" ]}
                         network,
                         capability,
                       }
                       ${lib.getExe' pkgs.openssh "ssh-copy-id"} {
                         ${lib.apparmor.generateFileRules [ "ssh" ]}
                         network,
                         capability,
                       }
                       ${lib.getExe' pkgs.openssh "sshfs"} {
                         ${lib.apparmor.generateFileRules [ "ssh" ]}
                         network,
                         capability,
                       }
                   '';
                  allRules.profile =
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
