{ config, lib, pkgs, home-manager-flake, ... }:

with lib;

let
  outerPkgs = pkgs;
  cfg = config.workContainer;
in {

  options = {

    workContainer = {
      enable = mkEnableOption (lib.mdDoc "Work container");
    };

    userName = mkOption {
      type = types.str;
      default = "florian";
      example = "florian";
      description = lib.mdDoc "User in the container";
    };

    xauth = mkOption {
      type = types.path;
      default = "/tmp/work_xauth";
      example = "/tmp/work_xauth";
      description = lib.mdDoc "xauth file to access host xserver";
    };

    uid = mkOption {
      type = types.int;
      default = config.users.users."${userName}".uid;
      example = "0";
      description = lib.mdDoc "xauth file to access host xserver";
    };
  };

  config = mkIf cfg.enable {

    networking = {
      nat = {
        enable = true;
        internalInterfaces = ["ve-+"];
        externalInterface = "+";
      };
      networkmanager = {
        unmanaged = [ "interface-name:ve-*" ];
      };
    };

    security.polkit = {
      enable = true;
      extraConfig = ''
        polkit.addRule(function(action, subject) {
            if (action.id == "org.freedesktop.machine1.shell" || action.id == "org.freedesktop.machine1.start") {
                if (subject.isInGroup("wheel")) {
                    return polkit.Result.YES;
                }
            }
        });
      '';
    };

    services.xserver.displayManager.sessionCommands = ''
    xauth nextract - "$DISPLAY" | sed -e 's/^..../ffff/' | xauth -f "${xauth}" nmerge -
    machinectl start work
  '';

    containers.work = {
      # TODO and bind for file transfer
      bindMounts = {
        x11Display = rec {
          hostPath = "/tmp/.X11-unix";
          mountPoint = hostPath;
          isReadOnly = true;
        };
        xAuthority = rec {
          hostPath = xauth;
          mountPoint = hostPath;
          isReadOnly = true;
        };
      };

      enableTun = true;
      privateNetwork = true;
      hostAddress = "192.168.10.1";
      localAddress = "192.168.10.2";

      config = {
        imports = [ home-manager-flake ];
        users.users."${userName}" = {
          extraGroups = lib.mkForce [];
          isNormalUser = true;
          shell = pkgs.zsh;
        };

        systemd.services.fix-nix-dirs = let
          profileDir = "/nix/var/nix/profiles/per-user/${userName}";
          gcrootsDir = "/nix/var/nix/gcroots/per-user/${userName}";
        in {
          script = ''
          #!${pkgs.stdenv.shell}
          set -euo pipefail

          mkdir -p ${profileDir} ${gcrootsDir}
          chown ${userName}:root ${profileDir} ${gcrootsDir}
        '';
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            Type = "oneshot";
          };
        };
        hardware.opengl = {
          enable = true;
          extraPackages = hostCfg.hardware.opengl.extraPackages;
        };

        environment.systemPackages = with pkgs; [
          git
          firefox
          remmina
        ];

        environment.etc."office.ovpn".source = ./office.ovpn;

        services.openvpn.servers.officeVPN = {
          config = '' config /etc/office.ovpn '';
          autoStart = true;
        };

        #      networking.defaultGateway.address = "192.168.10.1";
        networking.nameservers = [ "8.8.8.8" ];

        home-manager = {
          users."${cfg.userName}" = {
            imports = [
              ../../zsh.nix
              home/firefox.nix
            ];
            programs.zsh = {
              enable = true;
              oh-my-zsh.theme = "agnoster";
              shellAliases = {
                ll = "ls -a";
                "." ="cd ..";
                ".." = "cd ../..";
                "..." = "cd ../../..";
                "...." = "cd ../../../..";
              };
              enableAutosuggestions = true;
              oh-my-zsh = {
                enable = true;
                plugins = [ "vi-mode" "per-directory-history" ];
              };
            };
            programs.ssh.enable = true;
            home.sessionVariables = {
              SHELL = "zsh";
              DISPLAY = ":0";
              XDG_RUNTIME_DIR = "/run/user/${toString cfg.uid}";
              XAUTHORITY  = cfg.xauth;
            };
            home.stateVersion = "22.05";
            nixpkgs.overlays = outerPkgs.overlays;
          };
        };

        system.stateVersion = "22.05";
        nixpkgs.overlays = outerPkgs.overlays;
      };
    };
  };
}
