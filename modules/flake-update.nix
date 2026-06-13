{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.flake-remote-update;
  serviceName = "my-flake-update";
in

{
  options.flake-remote-update = {
    enable = mkEnableOption (lib.mdDoc "Whether to update flake");
    remote = mkOption {
      type = types.nullOr types.str;
      description = lib.mdDoc "Remote to fetch from";
    };

    credentials = mkOption {
      default = null;
      description = lib.mdDoc "credentials to access the remote";
      type = types.nullOr (
        types.submodule {
          options = {
            user = mkOption {
              default = null;
              type = types.nullOr types.str;
              description = lib.mdDoc "User to authenticate with";
            };
            passwordFile = mkOption {
              default = null;
              type = types.nullOr types.path;
              description = lib.mdDoc "File which contains the password or access token to connect to the remote";
            };
          };
        }
      );
    };

    ssh = mkOption {
      default = null;
      description = lib.mdDoc "ssh option for ssh remote";
      type = types.nullOr (
        types.submodule {
          options = {
            key = mkOption {
              type = types.path;
              description = lib.mdDoc "path to ssh key with access to the remote repository";
            };
            hostKey = mkOption {
              type = types.str;
              description = lib.mdDoc "name of host and its public key for fingerprint";
            };
          };
        }
      );
    };
    updateBranch = mkOption {
      type = types.str;
      description = lib.mdDoc "The branch which should be updated";
    };
    mainBranch = mkOption {
      default = "main";
      type = types.str;
      description = lib.mdDoc "The branch from which updates should occur";
    };

    dates = mkOption {
      type = types.str;
      example = "daily";
      description = lib.mdDoc ''
        How often or when updates occurs.

        The format is described in
        {manpage}`systemd.time(7)`.
      '';
    };

    randomizedDelaySec = mkOption {
      default = "0";
      type = types.str;
      example = "45min";
      description = lib.mdDoc ''
        Add a randomized delay before each automatic upgrade.
        The delay will be chosen between zero and this value.
        This value must be a time span in the format specified by
        {manpage}`systemd.time(7)`
      '';
    };

    persistent = mkOption {
      default = false;
      type = types.bool;
      example = true;
      description = lib.mdDoc ''
        Takes a boolean argument. If true, the time when the service
        unit was last triggered is stored on disk. When the timer is
        activated, the service unit is triggered immediately if it
        would have been triggered at least once during the time when
        the timer was inactive. Such triggering is nonetheless
        subject to the delay imposed by RandomizedDelaySec=. This is
        useful to catch up on missed runs of the service when the
        system was powered down.
      '';
    };
  };
  config = mkIf cfg.enable {
    systemd = {
      services."${serviceName}" = {
        environment = mkMerge [
          (mkIf (cfg.ssh != null) {
            GIT_SSH_COMMAND =
              let
                knownHostsFile = pkgs.writeText "knownHostsFile" cfg.ssh.hostKey;
              in
              "${pkgs.openssh}/bin/ssh -i $CREDENTIALS_DIRECTORY/sshKey -o GlobalKnownHostsFile=${knownHostsFile}";
          })
          (mkIf (cfg.credentials != null) {
            GIT_ASKPASS = pkgs.writeShellScript "askpass" ''
              case "$1" in
                  Username*)
                    echo ${cfg.credentials.user}
                    ;;
                  Password*)
                    cat $CREDENTIALS_DIRECTORY/passwordFile
                    ;;
              esac
            '';
          })
          {
            EMAIL = "<>";
            HOME = "/var/lib/${serviceName}";
            GIT_COMMITTER_NAME = "Update Service";
          }
        ];

        path =
          with pkgs;
          [
            nix
            git
          ]
          ++ (if cfg.ssh != null then [ openssh ] else [ ]);
        serviceConfig = mkMerge [
          {
            Type = "oneshot";
            CapabilityBoundingSet = "";
            LockPersonality = true;
            ProtectHostname = true;
            ProtectKernelLogs = true;
            ProtectKernelModules = true;
            ProtectKernelTunables = true;
            PrivateDevices = true;
            WorkingDirectory = "/var/lib/${serviceName}";
            StateDirectory = serviceName;
            RestrictNamespaces = true;
            RestrictRealtime = true;
            DynamicUser = true;
          }
          (mkIf (cfg.credentials != null) {
            LoadCredential = "passwordFile:${cfg.credentials.passwordFile}";
          })
          (mkIf (cfg.ssh != null) { LoadCredential = "sshKey:${cfg.ssh.key}"; })
        ];
        script = ''
          if [[ -d repo/.git ]]; then
            cd repo
            git fetch
          else
            git clone ${cfg.remote} repo
            cd repo
          fi
          git checkout -B ${cfg.updateBranch} origin/${cfg.mainBranch} 
          nix flake update --commit-lock-file
          git push --force-with-lease -o merge_request.create -o merge_request.target=${cfg.mainBranch} --set-upstream origin HEAD
        '';
        startAt = cfg.dates;
      };
      timers."${serviceName}".timerConfig = {
        RandomizedDelaySec = cfg.randomizedDelaySec;
        Persistent = cfg.persistent;
      };
    };
  };
}
