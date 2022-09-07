{ config, lib, pkgs, home-manager, ... }:

{
  # ...
  containers.graphicalExample = let
    userName = "work";
  in {
    # (1)
    bindMounts = {
      x11Display = rec {
        hostPath = "/tmp/.X11-unix";
        mountPoint = hostPath;
        isReadOnly = true;
      };
    };

    config = {
      imports = [
        (import "${home-manager}/nixos")
        ./desktop.nix
      ];

      # (4)
      users.users."${userName}".extraGroups = lib.mkForce [];

      # (5)
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
      # (2)
      hardware.opengl = {
        enable = true;
        extraPackages = hostCfg.hardware.opengl.extraPackages;
      };

      # (3)
      environment.systemPackages = with pkgs; [
        git
        firefox
      ];

      home-manager = {
        users."${userName}" = {
          imports = [ ../../../zsh.nix ];

          programs.ssh.enable = true;
          # (4)
          home.sessionVariables = {
            DISPLAY                             = ":0";
          };
        };
      };
    };
  };
}
